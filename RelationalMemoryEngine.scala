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



case class RelMemParams (
    regaddress: Int,
    rmeaddress: BigInt,
    mbus : MemoryBus
)





class RME(params: RelMemParams)(implicit p: Parameters) extends LazyModule
{

    val addr = Seq(AddressSet(params.rmeaddress, 0xfff))
    
    val device = new SimpleDevice("relmem",Seq("ku-csl,relmem")) 
    //{
    //  override def describe(resources: ResourceBindings): Description = {
    //    val res =  Map(("reserved" -> Seq(Binding(Some(this), ResourceAddress(Seq(addr), rocketchip.resources.ResourcePermissions(true, true, false, true, true))).value)))
    //    println(s"Resources: ${resources.toString}")
//
    //    val Description(name, mapping) = super.describe(resources)
//
//a
    //    Description(name, mapping ++ res)
    //  }
    //}
    val beatBytes = 8

    /*
      We need this to reserve an address range in the device tree 
    */
    ResourceBinding {
      Resource(device, "reg").bind(ResourceAddress(addr, rocketchip.resources.ResourcePermissions(true, true, false, true, true)))
    }
    


    
    //val access = TransferSizes(1, 64)
    //val xfer = TransferSizes(cache.blockBytes, cache.blockBytes)
    //val atom = TransferSizes(1, cache.beatBytes)
    //TLAdapterNode()
    val node = TLAdapterNode()
    
    //val node = TLAdapterNode(clientFn = { p =>
    //  // The ProbePicker assembles multiple clients based on the assumption they are contiguous in the clients list
    //  // This should be true for custers of xbar :=* BankBinder connections
    //  def combine(next: TLMasterParameters, pair: (TLMasterParameters, Seq[TLMasterParameters])) = {
    //    val (head, output) = pair
    //    if (head.visibility.exists(x => next.visibility.exists(_.overlaps(x)))) {
    //      (next, head +: output) // pair is not banked, push head without merging
    //    } else {
    //      def redact(x: TLMasterParameters) = x.v1copy(sourceId = IdRange(0,1), nodePath = Nil, visibility = Seq(AddressSet(0, ~0)))
    //      require (redact(next) == redact(head), s"${redact(next)} != ${redact(head)}")
    //      val merge = head.v1copy(
    //        sourceId = IdRange(
    //          head.sourceId.start min next.sourceId.start,
    //          head.sourceId.end   max next.sourceId.end),
    //        visibility = AddressSet.unify(head.visibility ++ next.visibility))
    //      (merge, output)
    //    }
    //  }
    //  val myNil: Seq[TLMasterParameters] = Nil
    //  val (head, output) = p.clients.init.foldRight((p.clients.last, myNil))(combine)
    //  p.v1copy(clients = head +: output)
    //},
    //managerFn = { p => p })
    //TLRegisterNode
    /* 
        We may need to shift the memory params so they do not overlap?
    */
    //val manager = TLManagerNode(Seq(TLSlavePortParameters.v1(Seq(TLManagerParameters(
    //address = Seq(AddressSet(params.rmeaddress, 0xfff)),
    //resources = device.reg,
    //regionType = RegionType.UNCACHED,
    //executable = false,
    //supportsGet        =  TransferSizes(1, 64),
    //supportsPutFull    =  TransferSizes(1, 64),
    //supportsPutPartial =  TransferSizes(1, 64),
    //supportsHint =        TransferSizes(1, 64),
    //fifoId = Some(0))), beatBytes)))

    
    //val identityNode = TLIdentityNode()
    //manager := identityNode 
    //node := identityNode

  //val regnode = new TLRegisterNode(
  //  address = Seq(AddressSet(params.regaddress, 0x7ff)),
  //  device = device,
  //  beatBytes = 8)

    /* 
    - if we set params.rmeaddress to be > memory limit then it never reaches here
    
    */

    def ToRME(addr : UInt) : Bool = {
        val torme : Bool = addr >= params.rmeaddress.U &&  addr <= (params.rmeaddress.U + 0xfff.U)
        torme
    }

    def UnmaskedAddress(addr: UInt) : UInt = {
        val unmaskedAddr = addr & ~params.rmeaddress.U
        unmaskedAddr
    }

     
  println("\n\n\n\nUsing relational memory engine\n\n\n\n")
  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val nClients = node.in.length
    require(nClients >= 1)
    println(s"Number of edges into RME: $nClients\n")
      

    for (i <- 0 until nClients)
    {
      val (out, out_edge) = node.out(i)
      val (in, in_edge) = node.in(i)
      val outParams = out_edge.bundle
      val inParams = in_edge.bundle

      out <> in


      //node.in.map{case (e, i) => println("client %s\n", i.params.)}
    



      val rme_in_queue = Module(new Queue(new TLBundleA(inParams), 128, flow=true))
      val rme_reply_queue = Module(new Queue(new TLBundleD(inParams), 128, flow=true))

    
      rme_in_queue.io.enq.valid := false.B
      rme_in_queue.io.enq.bits := in.a.bits
    

      when (ToRME(in.a.bits.address))
      {
          SynthesizePrintf("Received request to rme %x\n", in.a.bits.address)
          rme_in_queue.io.enq.bits := in.a.bits
          rme_in_queue.io.enq.valid := in.a.valid
          in.a.ready := rme_in_queue.io.enq.ready
      }

    

      // Pass between queues
      rme_in_queue.io.deq.ready := rme_reply_queue.io.enq.ready
      rme_reply_queue.io.enq.valid := rme_in_queue.io.deq.valid 
      val dReply = in_edge.AccessAck(rme_in_queue.io.deq.bits, 0x6969.U)
      rme_reply_queue.io.enq.bits := dReply
   

      // back to cache arbiter
      TLArbiter.robin(in_edge, in.d, out.d, rme_reply_queue.io.deq)
    }
    
    //TLArbiter.robin(out_edge, out.a, in.a, manager_in.a)

    //manager_in.d.bits := manager_in_edge.AccessAck(manager_in.a.bits, 0x6969.U)
    //manager_in.d.valid := manager_in_edge.done(manager_in.a)

  }

}

trait CanHaveRME extends {
  val rme: Option[RME]
}

trait HasRMEAttachable extends BaseSubsystem {
  val mbus = locateTLBusWrapper(MBUS)
//  mbus.coupleTo("rme-manager") {mbus.rme.get.manager := TLFragmenter(mbus.beatBytes, mbus.blockBytes) := _ }
}