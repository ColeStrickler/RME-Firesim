package subsystem.rme

package subsystem.rme
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLBundleA
import freechips.rocketchip.regmapper._
import freechips.rocketchip
import midas.targetutils.SynthesizePrintf
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import freechips.rocketchip.diplomacy.BufferParams.flow
import freechips.rocketchip.tilelink.TLMessages.AccessAck
import freechips.rocketchip.tilelink.TLMessages.AccessAckData
import freechips.rocketchip.diplomacy.{AddressRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.subsystem.{BaseSubsystem, MBUS, Attachable}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.subsystem.Attachable




class IDAllocator(minID : Int, maxID : Int) extends Module {
  val io = IO(new Bundle {
    val newID = Decoupled(UInt(log2Ceil(maxID).W))
    val retireID = Flipped(Decoupled(UInt(log2Ceil(maxID).W)))
  })

  val queue_depth = log2Ceil(maxID-minID+1)+1 // should always have room for retirement --> we don't check ready signal in ControlUnit
  val queue = Module(new Queue(UInt(log2Ceil(maxID).W), queue_depth))
  queue.io.enq <> io.retireID
  io.newID <> queue.io.deq

  val initDone = RegInit(false.B)
  val IDValues = VecInit((minID to maxID).map(i => i.U))
  val initCounter = RegInit(0.U(log2Ceil(queue_depth).W))


  when (!initDone)
  {
    queue.io.enq.bits := IDValues(initCounter)
    queue.io.enq.valid := true.B
    queue.io.deq.ready := false.B
    initCounter := Mux(queue.io.enq.fire, initCounter+1.U, initCounter)
    initDone := Mux(initCounter === (IDValues.length-1).U, true.B, false.B)
  }

}

/*
  Tile link diplomatic module that will increase the number of source IDs available to a downstream client.

  needed = new number of source IDs to accomodate
*/
class TLSourceExpander(needed: Int)(implicit p: Parameters)  extends LazyModule {
  def findBitsNeeded(baseWidth: Int, need: Int, check: Int) : Int = { // helper function to find bits needed
    assert(check <= 3) // this shouldnt happen
    if (math.pow(2,baseWidth + check) - math.pow(2, baseWidth) >= need)
      check
    else
      findBitsNeeded(baseWidth, need, check+1)
  }
  
  val node = (new TLAdapterNode(
    clientFn  = { cp => 
      val baseWidth = log2Ceil(cp.endSourceId)
      val new_width = math.pow(2, baseWidth+findBitsNeeded(baseWidth, needed, 1)).toInt
      val client = TLMasterParameters.v1(
        name     = "TLSourceExpander",
        sourceId = IdRange(0, new_width)
      )
      // We erase all client information since we crush the source Ids
      TLMasterPortParameters.v1(
        clients = Seq(client.v1copy(requestFifo = cp.clients.exists(_.requestFifo))),
        echoFields = cp.echoFields,
        requestFields = cp.requestFields,
        responseKeys = cp.responseKeys)
    },
    managerFn = { mp => mp.v1copy(managers = mp.managers.map(m => m.v1copy(fifoId = if (/*new_width==1*/false) Some(0) else m.fifoId))) // shouldn't happen
    }) {
    //override def circuitIdentity = edges.in.map(_.client).forall(noShrinkRequired)
  })


  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val len = node.in.length

    for (i <- 0 until len)
    {
      val (bundle, edge) = node.in(i)
      val (bundle_out, edge_out) = node.out(i)

      //when (bundle.a.valid)
      //{
      //  SynthesizePrintf("bundle.a.valid\n")
      //}
      bundle_out <> bundle
    }
  }
}


object TLSourceExpander {
  def apply(needed: Int)(implicit p: Parameters) : TLSourceExpander = {
    val SourceExpander = LazyModule(new TLSourceExpander(needed))
    SourceExpander
  }
}





class ConditionalDemuxD(params: TLBundleParameters) extends Module {
  val io = IO(new Bundle {
    val dataIn = Flipped(DecoupledIO(new TLBundleD(params))) // Single input (8-bit)
    val sel    = Input(Bool())   // Selector (1-bit)
    val outA   = Decoupled(new TLBundleD(params))// Output to location A
    val outB   = Decoupled(new TLBundleD(params)) // Output to location B
  })

  // Default both outputs to zero
  val readyOther = Reg(Bool()) // so we have somewhere to connect it to

  val dummyMessage = Wire(new TLBundleD(params))
  dummyMessage.opcode := 0.U
  dummyMessage.param := 0.U
  dummyMessage.size := 0.U
  dummyMessage.source := 0.U
  dummyMessage.data := 0.U
  dummyMessage.denied := 0.U
  dummyMessage.sink := 0.U
  dummyMessage.corrupt := false.B


  // Route input based on selector
  when(io.sel) {
    when (io.dataIn.fire)
    {
      SynthesizePrintf("from DRAM back to RME src %d\n", io.dataIn.bits.source)
    }
    
    io.outB <> io.dataIn
    io.outA.bits := dummyMessage
    io.outA.valid := false.B
    readyOther := io.outA.ready
  }.otherwise {
    when (io.dataIn.fire)
    {
      SynthesizePrintf("from DRAM skip RME src %d\n", io.dataIn.bits.source)
    }
    
    io.outA <> io.dataIn
    io.outB.bits := dummyMessage
    io.outB.valid := false.B
    readyOther := io.outB.ready
  }
}


class ConditionalDemuxA(params: TLBundleParameters) extends Module {
  val io = IO(new Bundle {
    val dataIn = Flipped(DecoupledIO(new TLBundleA(params))) // Single input (8-bit)
    val sel    = Input(Bool())   // Selector (1-bit)
    val outA   = DecoupledIO(new TLBundleA(params))// Output to location A
    val outB   = DecoupledIO(new TLBundleA(params)) // Output to location B
  })

  // Default both outputs to zero
  val readyOther = Reg(Bool()) // so we have somewhere to connect it to

  val dummyMessage = Wire(new TLBundleA(params))
  dummyMessage.opcode := 0.U
  dummyMessage.param := 0.U
  dummyMessage.size := 0.U
  dummyMessage.source := 0.U
  dummyMessage.address := 0.U
  dummyMessage.mask := 0.U
  dummyMessage.data := 0.U
  dummyMessage.corrupt := false.B

  when (io.sel)
  {
    SynthesizePrintf("Selector = 1\n")
  }



  // Route input based on selector
  when(io.sel) {
    io.outB <> io.dataIn
    io.outA.bits := dummyMessage
    io.outA.valid := false.B
    readyOther := io.outA.ready
    
  }.otherwise {
    io.outA <> io.dataIn
    io.outB.bits := dummyMessage
    io.outB.valid := false.B
    readyOther := io.outB.ready

  }
}













case class ScratchPadIO(MemDepth: Int, MemWidth: Int) extends Bundle
{
  val enable = Input(Bool())
  val write = Input(Bool())
  val addr = Input(UInt(log2Ceil(MemDepth).W))
  val dataIn = Input(UInt(MemWidth.W))
  val dataOut = Output(UInt(MemWidth.W))
}



class ScratchPadMemBank(MemDepth: Int, MemWidth: Int) extends Module {
  val io = IO(new ScratchPadIO(MemDepth, MemWidth))



  // ensure address not out of range
  assert(io.addr < MemDepth.U)

  // should be synthesized as SRAM
  val mem = SyncReadMem(MemDepth, UInt(MemWidth.W))
  
  
  // Create one write port and one read port
  mem.write(io.addr, io.dataIn)
  io.dataOut := mem.read(io.addr, io.enable)
}