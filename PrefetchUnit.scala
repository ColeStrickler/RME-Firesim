package subsystem.rme

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLBundleA
import freechips.rocketchip.regmapper._
import midas.targetutils.SynthesizePrintf
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import freechips.rocketchip.diplomacy.BufferParams.flow
import freechips.rocketchip.subsystem.{CacheBlockBytes}
import subsystem.rme._


case class SingleNextLinePrefetcherParams(
  ahead: Int = 4,
  waitForHit: Boolean = false,
  handleVA: Boolean = false
) 

class SnoopRME(implicit val p: Parameters) extends Bundle {
  val blockBytes = p(CacheBlockBytes)

  val write = Bool()
  val address = UInt()
  def block = address >> log2Up(blockBytes)
  def block_address = block << log2Up(blockBytes)
}

class PrefetchRME(implicit val p: Parameters) extends Bundle {
  val blockBytes = p(CacheBlockBytes)

  val write = Bool()
  val address = UInt()
  def block = address >> log2Up(blockBytes)
  def block_address = block << log2Up(blockBytes)
}

class PrefetcherIORME(implicit p: Parameters) extends Bundle {
  val snoop = Input(Valid(new SnoopRME))
  val request = Decoupled(new PrefetchRME)
  val hit = Output(Bool())
}

abstract class AbstractPrefetcherRME(implicit p: Parameters) extends Module {
  val io = IO(new PrefetcherIORME)

  io.request.valid := false.B
  io.request.bits := DontCare
  io.request.bits.address := 0.U(1.W)
  io.hit := false.B
}

class SingleNextLinePrefetcherRME(params: SingleNextLinePrefetcherParams)(implicit p: Parameters) extends AbstractPrefetcherRME()(p) {
  // Assume 4KB pages
  val lowerBits = 12 - log2Ceil(p(CacheBlockBytes))
  val s_idle :: s_wait :: s_active :: s_done :: Nil = Enum(4)
  val state = RegInit(s_idle)
  val write = Reg(Bool())
  val block_upper = Reg(UInt())
  val block_lower = Reg(UInt(lowerBits.W))
  val prefetch = Reg(UInt(lowerBits.W))
  val wrap = RegInit(false.B)
  val wrap_block_upper = block_upper + 1.U
  val delta = Mux(wrap, Cat(1.U(1.W), prefetch) - block_lower, prefetch - block_lower)

  val addr_hit = if (params.handleVA) {
    (io.snoop.bits.block >= Cat(block_upper, block_lower)) &&
    (io.snoop.bits.block <= Cat(Mux(wrap, wrap_block_upper, block_upper), prefetch))
  } else {
    (block_upper === io.snoop.bits.block >> lowerBits) &&
    (io.snoop.bits.block(lowerBits-1,0) >= block_lower) &&
    (io.snoop.bits.block(lowerBits-1,0) <= prefetch)
  }
  io.hit := state =/= s_idle && addr_hit

  val snoop_next_block = (io.snoop.bits.block(lowerBits-1,0) + 1.U)(lowerBits-1,0)

  when ((state === s_idle || (state === s_wait && !io.hit)) && io.snoop.valid) {
    when (~io.snoop.bits.block(lowerBits-1,0) =/= 0.U || params.handleVA.B) {
      state := (if (params.waitForHit) s_wait else s_active)
    }
    block_upper := io.snoop.bits.block >> lowerBits
    block_lower := io.snoop.bits.block
    prefetch := snoop_next_block
    when (params.handleVA.B && !io.hit) {
      wrap := snoop_next_block === 0.U
    }
    write := io.snoop.bits.write
  }

  io.request.valid := state === s_active
  io.request.bits.write := write
  io.request.bits.address := Cat(Mux(wrap, wrap_block_upper, block_upper), prefetch) << log2Up(io.request.bits.blockBytes)


  when (io.request.fire) {
    prefetch := prefetch + 1.U
    when (prefetch === ~(0.U(lowerBits.W))) {
      if (params.handleVA) {
        state := Mux(delta >= params.ahead.U, s_wait, s_active)
        prefetch := 0.U
        wrap := true.B
      } else {
        state := s_done
        prefetch := prefetch
      }
    } .elsewhen (delta >= params.ahead.U) {
      state := s_wait
    } .otherwise {
      state := s_active
    }
  }
  when (state === s_done && block_lower === prefetch) {
    state := s_idle
  }

  when (io.hit && io.snoop.valid) {
    when (state =/= s_done && io.snoop.bits.block =/= Cat(block_upper, block_lower)) {
      state := s_active
    }
    write := io.snoop.bits.write
    block_lower := io.snoop.bits.block
    block_upper := io.snoop.bits.block >> lowerBits
    when (io.snoop.bits.block(lowerBits-1,0) === prefetch) {
      prefetch := prefetch + 1.U
    }
    when (wrap && ((io.snoop.bits.block >> lowerBits) =/= block_upper)) {
      wrap := false.B
    }
  }
}

case class RequestorInjectionRequest(params: RelMemParams) extends Bundle {
    val RequestAddr = Input(UInt(log2Ceil(params.rmeAddressSize).W))
    val InjectionReqNum = Input(UInt(log2Ceil(32).W)) // for now we will just use this to select
}


case class PrefetchUnitAGUIO(params: RelMemParams) extends Bundle {
   // val AsyncInjectionRequest = Flipped(Valid(UInt(log2Ceil(params.rmeAddressSize).W)))
    val InjectionRequest = Flipped(Decoupled(new RequestorInjectionRequest(params)))
    val Injection = Valid(Output(UInt(32.W)))
    val config_StreamPhysRegisters = Input(Vec(1, UInt(64.W)))
    val config_StreamDataSize = Input(UInt(32.W))
}


case class PrefetchUnitFetchUnitPortIn() extends Bundle
{
  val data = UInt(512.W) // 64 bytes = 1 cache line
  val addr = UInt(33.W) // will take out of the reqTableEntry
  val config = UInt(4.W)
}

case class PrefetchUnitFetchUnitPort(inMaxID:Int, outmaxID : Int) extends Bundle
{
  val ToFetchUnit = DecoupledIO(new RequestorFetchUnitPort(inMaxID, outmaxID))
  val ToPre = Flipped(Decoupled(new PrefetchUnitFetchUnitPortIn()))
}

case class FetchUnitPrefetchUnitPort(inMaxID:Int, outmaxID : Int) extends Bundle
{
  //val ToFetchUnit = DecoupledIO(new RequestorFetchUnitPort(inMaxID, outmaxID))
  val ToPre = Flipped(Decoupled(new PrefetchUnitFetchUnitPortIn()))
}





case class PreFetchUnitIO(params: RelMemParams, inMaxID : Int, outmaxID : Int) extends Bundle {
    val Requestor = new PrefetchUnitAGUIO(params)
    val FetchUnit = new PrefetchUnitFetchUnitPort(inMaxID, outmaxID)
}





/*
    Track outbound requests
*/
case class PrefetchOutboundTableEntry(params: RelMemParams) extends Bundle {
    val address = UInt(log2Ceil(params.rmeAddressSize).W)
    val valid = Bool()
}



object DataState extends ChiselEnum {
  val Available, Requested, NeedRequest = Value
}

class PreFetchUnitRME(params: RelMemParams, tlInEdge : TLEdge, tlOutEdge: TLEdge, tlOutBundle: TLBundle, config: Int)(
    implicit p: Parameters) extends Module
{  
    val tlOutParams = tlOutEdge.bundle
    val tlInParams = tlInEdge.bundle
    val outMaxID = (math.pow(2, tlOutParams.sourceBits)-1).toInt
    val inMaxID = (math.pow(2, tlInParams.sourceBits)-1).toInt
    val depthAhead = 4

    val io = IO(new PreFetchUnitIO(params, inMaxID, outMaxID))



    


    def InjectionPacketAsHalfWords(injectionPacket: UInt): Vec[UInt] = {
        injectionPacket.asTypeOf(Vec(32, UInt(16.W)))
    } 

    def InjectionPacketAsWords(injectionPacket: UInt): Vec[UInt] = {
        injectionPacket.asTypeOf(Vec(16, UInt(32.W)))
    } 

    

    
    val nInjectionPackets = depthAhead * 2

    val injectionPackets =
      RegInit(VecInit(Seq.fill(nInjectionPackets)(0.U(512.W))))

    val injectionPacketAddr =
      RegInit(VecInit(Seq.fill(nInjectionPackets)(0.U(28.W))))

    val injectionPacketsValid =
      RegInit(VecInit(Seq.fill(nInjectionPackets)(false.B)))
    val stream2PhysicalAddressStart = RegInit((params.rmeaddress + 0x08000000L).U(33.W)) // pointed to the corresponding metadata stream start 
    stream2PhysicalAddressStart := io.Requestor.config_StreamPhysRegisters(0)
    def CheckRequestorReqPresentPacketTable(reqAddr: UInt): (Bool, UInt) = {
    val matches = injectionPacketAddr.zip(injectionPacketsValid).map {
      case (entryAddr, valid) =>
        valid && entryAddr === reqAddr
    }

    (matches.reduce(_ || _), PriorityEncoder(matches))
  }

    val OldestInjectionPacket = RegInit(0.U(log2Ceil(depthAhead*2).W))
    def IncOldestInjectionPacket() : Unit = {
        OldestInjectionPacket := Mux(OldestInjectionPacket === (depthAhead*2-1).U, 0.U, OldestInjectionPacket+1.U)
    }
    def AllocateEntryInjectionPacketTable(data: UInt, addr: UInt) : Unit = {
      injectionPackets(OldestInjectionPacket) := data
      injectionPacketAddr(OldestInjectionPacket) := (addr-stream2PhysicalAddressStart)
      injectionPacketsValid(OldestInjectionPacket) := true.B
      IncOldestInjectionPacket()
    }



    val OutBoundReqTable =
        RegInit(VecInit(Seq.fill(depthAhead*2)(
            0.U.asTypeOf(new PrefetchOutboundTableEntry(params))
        )))

    def OutBoundReqMatchAddr(addr: UInt) : IndexedSeq[Bool] = {
      OutBoundReqTable.map(e => (e.address === addr) && e.valid)
    }

    def CheckRequestReqPresentOutboundTable(addr: UInt): Bool = {
            OutBoundReqMatchAddr(addr)
                .reduce(_ || _)
        }

    
    def AllocateTableEntry(addr: UInt) : Unit = {
      val entryIdx = PriorityEncoder(OutBoundReqTable.map(!_.valid))
      OutBoundReqTable(entryIdx).address := addr
      OutBoundReqTable(entryIdx).valid := true.B
    }

    def FreeTableEntry(addr: UInt) : Unit = {
      val entryIdx = PriorityEncoder(OutBoundReqMatchAddr(addr))
      OutBoundReqTable(entryIdx).valid := false.B
    }


    def MakeReqDescriptor(addr: UInt): RequestDescriptor = {
        val descriptorOut = Wire(new RequestDescriptor(inMaxID, outMaxID))
        descriptorOut.baseID := 0.U
        descriptorOut.requestPlacement := config.U // reuse this field to support route back
        descriptorOut.done := false.B
        descriptorOut.dst := DESTINATION.PREFETCH_UNIT
        descriptorOut.addr := addr + stream2PhysicalAddressStart
        descriptorOut
    }
    




    val PrefetcherRME = Module(new SingleNextLinePrefetcherRME(SingleNextLinePrefetcherParams(ahead=depthAhead, handleVA=true)))
    val DownstreamReqQueue = Module(new Queue(new RequestDescriptor(inMaxID, outMaxID), depthAhead, flow=false)) // can maybe just make this an address, and make descriptor on way out
    val RequestQueueArb = Module(new RRArbiter(new RequestDescriptor(inMaxID, outMaxID), 2))

    when (PrefetcherRME.io.request.fire)
    {
      //SynthesizePrintf("[PrefetcherRME] PREFETCHER FIRE 0x%x\n", PrefetcherRME.io.request.bits.address)
    }

  
    RequestQueueArb.io.in(0).bits := MakeReqDescriptor(PrefetcherRME.io.request.bits.address)
    RequestQueueArb.io.in(0).valid := PrefetcherRME.io.request.valid
    PrefetcherRME.io.request.ready := RequestQueueArb.io.in(0).ready

    val InjectionReqAddr = Mux(io.Requestor.InjectionRequest.bits.RequestAddr < (params.rmeAddressSize).U, io.Requestor.InjectionRequest.bits.RequestAddr, 0.U) 
    RequestQueueArb.io.in(1).bits := MakeReqDescriptor(InjectionReqAddr)
    RequestQueueArb.io.in(1).valid := false.B

    DownstreamReqQueue.io.enq <> RequestQueueArb.io.out


    PrefetcherRME.io.snoop.valid :=
      io.Requestor.InjectionRequest.fire
 

    PrefetcherRME.io.snoop.bits.write := false.B
    PrefetcherRME.io.snoop.bits.address := io.Requestor.InjectionRequest.bits.RequestAddr
      io.Requestor.Injection.bits := 0.U
    val state = RegInit(DataState.Available)

    io.Requestor.Injection.valid := false.B
    io.Requestor.InjectionRequest.ready := false.B // default

    when (io.FetchUnit.ToPre.fire)
    {
      AllocateEntryInjectionPacketTable(io.FetchUnit.ToPre.bits.data, io.FetchUnit.ToPre.bits.addr)
     // SynthesizePrintf("io.FetchUnit.ToPre.fire 0x%x -- 0x%x\n", io.FetchUnit.ToPre.bits.data, io.FetchUnit.ToPre.bits.addr)
     // SynthesizePrintf("io.FetchUnit.ToPre.fire --> data 0x%x\n", io.FetchUnit.ToPre.bits.data)
      for (i <- 0 until depthAhead*2) {
        //SynthesizePrintf(
        //  "packet_table[%d] addr=0x%x valid=%d data 0x%x\n",
        //  i.U,
        //  injectionPacketAddr(i),
        //  injectionPacketsValid(i),
        //  injectionPackets(i)
        //)
      }
    }


    // Request to FetchUnit
    io.FetchUnit.ToFetchUnit.valid := DownstreamReqQueue.io.deq.valid
    DownstreamReqQueue.io.deq.ready := io.FetchUnit.ToFetchUnit.ready
    io.FetchUnit.ToFetchUnit.bits.descriptor := DownstreamReqQueue.io.deq.bits
    io.FetchUnit.ToFetchUnit.bits.extractionDescriptor := 0.U.asTypeOf(io.FetchUnit.ToFetchUnit.bits.extractionDescriptor)
    io.FetchUnit.ToPre.ready := true.B // for now we will just accept everything

    // When new injection request
    when (io.Requestor.InjectionRequest.valid)
    {
       SynthesizePrintf("InjectionRequest.valid 0x%x\n", io.Requestor.InjectionRequest.bits.RequestAddr)
        /*
            States:
                1. Data is available
                2. Data has been requested in outbound request
                3. We need to request the data
        
        */
        
        val presentOutBoundTable = CheckRequestReqPresentOutboundTable(InjectionReqAddr)
        val (isPresentDataCache, dataCacheIdx) = CheckRequestorReqPresentPacketTable(InjectionReqAddr)

        when (presentOutBoundTable || isPresentDataCache)
        {
         // SynthesizePrintf("InjectionRequest hit! 0x%x\n", InjectionReqAddr)
        }


        when(io.FetchUnit.ToFetchUnit.fire)
        {
        //    SynthesizePrintf("(PrefetchUnit[%d].ToFetchUnit.fire) 0x%x\n", config.U, io.FetchUnit.ToFetchUnit.bits.descriptor.addr)   
        }
        //when (io.FetchUnit.ToPre.fire)
        //{
        //    SynthesizePrintf("(PrefetchUnit[%d].ToPre.fire)\n", config.U)   
        //}



        when (io.Requestor.InjectionRequest.fire)
        {
          //  SynthesizePrintf("(PrefetchUnit[%d].InjectionRequest.fire) 0x%x\n", config.U, io.Requestor.InjectionRequest.bits.RequestAddr)  
        }


        switch (state)
        {
          is (DataState.Available)
          {
            when(isPresentDataCache)
            {
              val data = InjectionPacketAsWords(injectionPackets(dataCacheIdx))(io.Requestor.InjectionRequest.bits.InjectionReqNum)
              SynthesizePrintf("isPresentDataCache %d 0x%x\n", io.Requestor.InjectionRequest.bits.InjectionReqNum, data)

              io.Requestor.InjectionRequest.ready := true.B
              io.Requestor.Injection.bits := data
              io.Requestor.Injection.valid := true.B

            }.elsewhen(presentOutBoundTable)
            {
             // SynthesizePrintf("PrefetchUnitCurrentState %d present in OutBoundTable\n", state.asUInt)
              val pred = io.FetchUnit.ToPre.fire && (io.FetchUnit.ToPre.bits.addr-stream2PhysicalAddressStart) === InjectionReqAddr
              state := Mux(pred, DataState.Available, DataState.Requested) // ensure we 
              when(pred)
              {
                FreeTableEntry(InjectionReqAddr)
              }

            }
            .otherwise
            {
            //  SynthesizePrintf("PrefetchUnitCurrentState %d NeedRequest\n", state.asUInt)
              state := DataState.NeedRequest
            }
          }

          is (DataState.Requested)
          {
           // SynthesizePrintf("PrefetchUnit CurrentState=Requested\n")
            val pred = io.FetchUnit.ToPre.fire && (io.FetchUnit.ToPre.bits.addr-stream2PhysicalAddressStart) === InjectionReqAddr
            state := Mux(pred, DataState.Available, DataState.Requested)
            when(pred)
            {
              FreeTableEntry(InjectionReqAddr)
            }            
          }

          is (DataState.NeedRequest)
          {
           // SynthesizePrintf("PrefetchUnit NeedRequest %d\n", RequestQueueArb.io.in(1).fire)
            RequestQueueArb.io.in(1).valid := true.B
            val pred = RequestQueueArb.io.in(1).fire
            state := Mux(pred, DataState.Requested, DataState.NeedRequest)
            AllocateTableEntry(InjectionReqAddr)
          }
        }
    }


}