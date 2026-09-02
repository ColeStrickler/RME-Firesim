package subsystem.rme

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import midas.targetutils.SynthesizePrintf

/**
  * A small, register-backed cache for cache-line reads issued by FetchUnitRME.
  *
  * Misses pass through to memory and their D-channel data is recorded. Hits do
  * not issue an A-channel request; instead, the cached line is replayed on the
  * D channel with the source ID from the new request.
  */
class FetchRequestCache(
    tlParams: TLBundleParameters,
    entries: Int,
    lineBytes: Int = 64) extends Module {

  require(entries > 0)
  require(isPow2(lineBytes))
  require(tlParams.dataBits % 8 == 0)

  private val beatBytes = tlParams.dataBits / 8
  require(lineBytes % beatBytes == 0)

  private val beatsPerLine = lineBytes / beatBytes
  private val beatBits = math.max(1, log2Ceil(beatsPerLine))
  private val ageBits = math.max(1, log2Ceil(entries))
  private val sourceCount = 1 << tlParams.sourceBits
  private val lineOffsetBits = log2Ceil(lineBytes)

  val io = IO(new Bundle {
    val fetchReq = Flipped(Decoupled(new TLBundleA(tlParams)))
    val memReq = Decoupled(new TLBundleA(tlParams))

    val memResp = Flipped(Decoupled(new TLBundleD(tlParams)))
    val fetchResp = Decoupled(new TLBundleD(tlParams))
    val flush = Input(Bool())
  })

  private def lineAddress(address: UInt): UInt =
    Cat(address(address.getWidth - 1, lineOffsetBits), 0.U(lineOffsetBits.W))

  val valid = RegInit(VecInit(Seq.fill(entries)(false.B)))
  val tag = Reg(Vec(entries, UInt(tlParams.addressBits.W)))
  val data = Reg(Vec(entries, Vec(beatsPerLine, UInt(tlParams.dataBits.W))))

  // Rank 0 is most recently used; the largest rank is least recently used.
  val age = RegInit(VecInit((0 until entries).map(_.U(ageBits.W))))

  val requestLine = lineAddress(io.fetchReq.bits.address)
  val cacheable = io.fetchReq.bits.opcode === TLMessages.Get &&
    io.fetchReq.bits.size === log2Ceil(lineBytes).U
  val hitVec = VecInit((0 until entries).map(i => valid(i) && tag(i) === requestLine))
  val hit = cacheable && hitVec.asUInt.orR && !io.flush
  val hitIndex = PriorityEncoder(hitVec)

  val invalidVec = VecInit(valid.map(v => !v))
  val hasInvalid = invalidVec.asUInt.orR
  val lruAge = age.reduce((a, b) => Mux(a > b, a, b))
  val lruVec = VecInit(age.map(_ === lruAge))
  val replacementIndex = Mux(hasInvalid, PriorityEncoder(invalidVec), PriorityEncoder(lruVec))

  def markMostRecentlyUsed(index: UInt, wasValid: Bool): Unit = {
    val oldAge = age(index)
    for (i <- 0 until entries) {
      when(valid(i) && i.U =/= index && (!wasValid || age(i) < oldAge)) {
        age(i) := age(i) + 1.U
      }
    }
    age(index) := 0.U
  }

  // Metadata used to associate returning D beats with an earlier miss.
  val missValid = RegInit(VecInit(Seq.fill(sourceCount)(false.B)))
  val missTag = Reg(Vec(sourceCount, UInt(tlParams.addressBits.W)))
  val missBeat = RegInit(VecInit(Seq.fill(sourceCount)(0.U(beatBits.W))))
  val missData = Reg(Vec(sourceCount, Vec(beatsPerLine, UInt(tlParams.dataBits.W))))

  val replayActive = RegInit(false.B)
  val replayEntry = Reg(UInt(math.max(1, log2Ceil(entries)).W))
  val replaySource = Reg(UInt(tlParams.sourceBits.W))
  val replaySize = Reg(UInt(tlParams.sizeBits.W))
  val replayEcho = Reg(chiselTypeOf(io.fetchReq.bits.echo))
  val replayBeat = RegInit(0.U(beatBits.W))

  // Do not begin a cached response in the middle of a pass-through D burst.
  val memBurstActive = RegInit(false.B)
  val responseSource = io.memResp.bits.source
  val responseTracked = missValid(responseSource)
  val responseLast = responseTracked && missBeat(responseSource) === (beatsPerLine - 1).U

  io.memReq.valid := io.fetchReq.valid && !hit && !replayActive
  io.memReq.bits := io.fetchReq.bits

  val canStartReplay = !replayActive && !memBurstActive && !io.memResp.valid
  io.fetchReq.ready := Mux(hit, canStartReplay, !replayActive && io.memReq.ready)

  when(io.fetchReq.fire && hit) {
    SynthesizePrintf(
      "[FetchRequestCache] hit line=0x%x entry=%d source=%d\n",
      requestLine,
      hitIndex,
      io.fetchReq.bits.source)
    replayActive := true.B
    replayEntry := hitIndex
    replaySource := io.fetchReq.bits.source
    replaySize := io.fetchReq.bits.size
    replayEcho := io.fetchReq.bits.echo
    replayBeat := 0.U
    markMostRecentlyUsed(hitIndex, true.B)
  }

  when(io.memReq.fire && cacheable) {
    val source = io.memReq.bits.source
    assert(!missValid(source), "FetchRequestCache reused a source ID with a miss outstanding")
    missValid(source) := true.B
    missTag(source) := requestLine
    missBeat(source) := 0.U
  }

  val replayResponse = WireDefault(0.U.asTypeOf(new TLBundleD(tlParams)))
  replayResponse.opcode := TLMessages.AccessAckData
  replayResponse.param := 0.U
  replayResponse.size := replaySize
  replayResponse.source := replaySource
  replayResponse.sink := 0.U
  replayResponse.denied := false.B
  replayResponse.data := data(replayEntry)(replayBeat)
  replayResponse.corrupt := false.B
  replayResponse.echo := replayEcho

  io.fetchResp.valid := Mux(replayActive, true.B, io.memResp.valid)
  io.fetchResp.bits := Mux(replayActive, replayResponse, io.memResp.bits)
  io.memResp.ready := io.fetchResp.ready && !replayActive

  when(replayActive && io.fetchResp.fire) {
    when(replayBeat === (beatsPerLine - 1).U) {
      replayActive := false.B
      replayBeat := 0.U
    }.otherwise {
      replayBeat := replayBeat + 1.U
    }
  }

  when(io.memResp.fire && responseTracked) {
    missData(responseSource)(missBeat(responseSource)) := io.memResp.bits.data
    memBurstActive := !responseLast

    when(responseLast) {
      // A second outstanding miss for this line may have filled it already.
      // Update that entry instead of creating duplicate cache entries.
      val fillMatchVec = VecInit((0 until entries).map(i =>
        valid(i) && tag(i) === missTag(responseSource)))
      val fill = Mux(fillMatchVec.asUInt.orR, PriorityEncoder(fillMatchVec), replacementIndex)
      val fillWasValid = valid(fill)
      missValid(responseSource) := false.B
      missBeat(responseSource) := 0.U

      // Only successful read-data responses are reusable cache entries.
      when(io.memResp.bits.opcode === TLMessages.AccessAckData &&
          !io.memResp.bits.denied && !io.memResp.bits.corrupt) {
        valid(fill) := true.B
        tag(fill) := missTag(responseSource)
        for (beat <- 0 until beatsPerLine) {
          data(fill)(beat) := Mux(
            missBeat(responseSource) === beat.U,
            io.memResp.bits.data,
            missData(responseSource)(beat))
        }
        markMostRecentlyUsed(fill, fillWasValid)
      }
    }.otherwise {
      missBeat(responseSource) := missBeat(responseSource) + 1.U
    }
  }

  when(io.flush) {
    valid.foreach(_ := false.B)
  }
}
