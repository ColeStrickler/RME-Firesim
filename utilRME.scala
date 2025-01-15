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




class ConditionalDemux(params: TLBundleParameters) extends Module {
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

  // if this never fires we have a problem
  when (io.dataIn.valid )
  {
    SynthesizePrintf("io.dataIn.valid\n")
    SynthesizePrintf("io.data.bits.address 0x%x\n", io.dataIn.bits.address)
  }

  when (!io.outB.ready)
  {
    SynthesizePrintf("!io.dataIn.ready\n")
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