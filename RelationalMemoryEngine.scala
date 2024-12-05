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
import freechips.rocketchip.tilelink.TLMessages.AccessAck
import freechips.rocketchip.tilelink.TLMessages.AccessAckData


case class RelMemParams (
    regaddress: Int,
    rmeaddress: BigInt,
)





class RME(params: RelMemParams)(implicit p: Parameters) extends LazyModule
{
    val device = new SimpleDevice("relmem",Seq("ku-csl,relmem"))

    val node = TLAdapterNode()
    val beatBytes = 8
    val (out, out_edge) = node.out(0)
    val (in, in_edge) = node.in(0)
    val outParams = out_edge.bundle
    val inParams = in_edge.bundle
    val slaveParams = node.managerFn
    /* 
        We may need to shift the memory params so they do not overlap?
    */
    val manager = TLManagerNode(Seq(TLSlavePortParameters.v1(Seq(TLManagerParameters(
    address = Seq(AddressSet(params.rmeaddress, 0xfff)),
    resources = device.reg,
    regionType = RegionType.CACHED,
    executable = false,
    supportsArithmetic = TransferSizes(1, beatBytes),
    supportsLogical = TransferSizes(1, beatBytes),
    supportsGet = TransferSizes(1, beatBytes),
    supportsPutFull = TransferSizes(1, beatBytes),
    supportsPutPartial = TransferSizes(1, beatBytes),
    supportsHint = TransferSizes(1, beatBytes),
    fifoId = Some(0))), beatBytes)))

    
    val (manager_out, manager_out_edge) = manager.out(0)
    val (manager_in, manager_in_edge) = manager.in(0)
    val managerInParams = manager_in_edge.bundle
    val managerOutParams = manager_out_edge.bundle



  val regnode = new TLRegisterNode(
    address = Seq(AddressSet(params.regaddress, 0x7ff)),
    device = device,
    beatBytes = 8)



    def ToRME(addr : UInt) : Bool = {
        val torme : Bool = addr > params.rmeaddress.U
        torme
    }
     
  println("\n\n\n\nUsing relational memory engine\n\n\n\n")
  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val nClients = node.in.length
    println(s"Number of edges into RME: $nClients ${node.in}")
    require(nClients >= 1)
    
    val memBase = p(ExtMem).get.master.base.U
    // last connect semantics should work for us here

    /*
        For now lets just let everything flow through
    */
    out <> in


    // All incoming requests are channel A right? So we should only give D responses
    // We will alternate who sends data back
    //val returnArbiter = Module(new TLArbiter(new TLBundleD(inParams), 2))
    
    //val rmeInQueue = Module(new Queue(new TLBundleA(managerInParams), 16))
    //rmeInQueue.io.enq.valid := manager_in.a.fire
    //rmeInQueue.io.enq := manager_in.a

    // reply with fixed data
    when (manager_in.a.fire)
    {
        SynthesizePrintf("Received request to manager: %d\n", manager_in.a.bits.address)
    }

    manager_in.d := manager_in_edge.AccessAck(manager_in.a.bits, 0x6969.U)
    manager_in.d.valid := manager_in_edge.done(manager_in.a)
    /*
        We arbitrate on the out edge between requests from the RME and other requests
    */
    //TLArbiter.robin(out_edge, out.a, ou)
  }

}

trait CanHaveRME {
  val rme: Option[RME]
}