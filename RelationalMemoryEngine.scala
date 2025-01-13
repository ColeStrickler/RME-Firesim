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
    mbus : MemoryBus,
    controlMMIOAddress : Int,
    controlBeatBytes : Int
)






class RME(params: RelMemParams)(implicit p: Parameters) extends LazyModule
{

    val addr = Seq(AddressSet(params.rmeaddress, 0xfff))
    
    val device = new SimpleDevice("relmem",Seq("ku-csl,relmem")) with HasReservedAddressRange {

    }

    /*
      We need this to reserve an address range in the device tree 
      -> This required modifications to the device tree generation. See RocketChip fork
    */
    ResourceBinding {
      Resource(device, "reserved").bind(ResourceAddress(addr, rocketchip.resources.ResourcePermissions(true, true, false, true, true)))
    }
    

    val node = TLAdapterNode()
    


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
    val ConfigPort = ConfigurationPortRME(params, device)

    for (i <- 0 until nClients)
    {
      val (out, out_edge) = node.out(i)
      val (in, in_edge) = node.in(i)
      val outParams = out_edge.bundle
      val inParams = in_edge.bundle
      out.b <> in.b
      out.c <> in.c
      out.e <> in.e
      println(s"Client #$i Name: ${in_edge.client.clients(0).name}")
      println(s"in.d.numBeats ${in_edge.numBeats(in.d.bits)}\n")
     val inDBeats = in_edge.numBeats(in.d.bits)

      val demux = Module(new ConditionalDemux(inParams))
      val rme_in_queue = Module(new Queue(new TLBundleA(inParams), 128, flow=false))
      val rme_reply_queue = Module(new Queue(new TLBundleD(inParams), 128, flow=false))


      
      //rme_in_queue.io.enq <> in.a // everything passes through queue
      //// defaults
      //rme_in_queue.io.enq.valid := false.B
      //rme_in_queue.io.enq.bits := in.a.bits
      when (in.a.fire)
      {
        // When in the rme address range we pass here
        // handle requests in
        SynthesizePrintf("in.a.fire: 0x%x\n", in.a.bits.address)
        //rme_in_queue.io.enq <> in.a
        
      }


      when (rme_in_queue.io.deq.valid)
      {
        SynthesizePrintf("rme_in_queue.io.deq.valid = 1\n")
        SynthesizePrintf("rme_reply_queue.io.enq.ready = %d\n", rme_reply_queue.io.enq.ready)
        SynthesizePrintf("rme_in_queue.io.deq.bits.address 0x%x\n", rme_in_queue.io.deq.bits.address)
      }


      val currentRequest = Wire(Decoupled(new TLBundleD(inParams)))
      val (d_first, d_last, d_done) = in_edge.firstlast(currentRequest)
      val currentlyBeating = RegInit(false.B)
      val toSend = Reg(new TLBundleD(inParams))
      
      currentlyBeating := Mux(currentlyBeating, !d_last, rme_reply_queue.io.deq.fire)
      rme_reply_queue.io.deq.ready := !currentlyBeating
      
      when (rme_reply_queue.io.deq.fire)
      {
        toSend <> rme_reply_queue.io.deq.bits
      }

      currentRequest.bits <> toSend
      currentRequest.valid := currentlyBeating

      //currentlyBeating := d_first || (currentlyBeating && (beatCounter =/= inDBeats)) 
      rme_reply_queue.io.enq.valid := rme_in_queue.io.deq.valid 
      rme_in_queue.io.deq.ready := rme_reply_queue.io.enq.ready


      val dReply = in_edge.AccessAck(rme_in_queue.io.deq.bits, 0x6969.U)
      println("dReply.size: %d\n", dReply.size)
      
      rme_reply_queue.io.enq.bits := dReply



      when (ToRME(in.a.bits.address))
      {
        SynthesizePrintf("Setting selector to RME\n")
      }
    

      val isRMERequest = ToRME(in.a.bits.address) && (in.a.bits.opcode === TLMessages.Get)
      

      demux.io.dataIn <> in.a
      demux.io.sel := isRMERequest
      out.a <> demux.io.outA
      rme_in_queue.io.enq <> demux.io.outB

      when (demux.io.outB.valid)
      {
        SynthesizePrintf("demux.io.outB.valid: %d\n", demux.io.outB.valid)
        SynthesizePrintf("rme_in_queue.io.ready %d\n", rme_in_queue.io.enq.ready)
      }
      

      when (!rme_in_queue.io.enq.ready)
      {
        SynthesizePrintf("!rme_in_queue.io.enq.valid --> CHECK\n")
      }


      when (ToRME(in.a.bits.address) && (in.a.bits.opcode =/= TLMessages.Get))
      {
          SynthesizePrintf("Received non-Get request to RME 0x%x\n", in.a.bits.opcode)
      }
      
      //CYCLE:  22990723637 io.dataIn.valid
      //CYCLE:  22990723637 demux.io.outB.valid: 1
      //CYCLE:  22990723637 Selector = 1
      //CYCLE:  22990723637 Setting selector to RME
      //CYCLE:  22990723637 rme_in_queue.io.ready 1
      //CYCLE:  22990723638 rme_in_queue.io.deq.valid = 1
      //CYCLE:  22990723638 rme_in_queue.io.deq.bits.address 0x110000000
      //CYCLE:  22990723638 rme_reply_queue.io.enq.ready = 1
      //CYCLE:  22990723638 RME Got in queue entry   1
      //CYCLE:  22990723639 Firing request out of reply queue
      when (ToRME(out.a.bits.address) && out.a.fire)
      {
        // When in the rme address range we pass here
        // handle requests in
        SynthesizePrintf("BAD! out request should've went to RME %x\n", out.a.bits.address)
        //rme_in_queue.io.enq <> in.a
        
      }
      //.otherwise
      //{
      //  
      //}

      when (rme_reply_queue.io.deq.fire)
      {
        
        SynthesizePrintf("Firing request out of reply queue\n")
        SynthesizePrintf("d_first %d, d_last %d, d_done %d\n", d_first, d_last, d_done)
      }
      when (rme_reply_queue.io.count > 0.U)
      {
        SynthesizePrintf("RME Got reply queue entry %d\n", rme_reply_queue.io.count)
      }
      when (rme_in_queue.io.count > 0.U)
      {
        SynthesizePrintf("RME Got in queue entry %d\n", rme_in_queue.io.count)
      }

      

     

      

      



      // back to cache arbiter
      // previously was taking directly from the reply queue before trying to implement beats
      TLArbiter.robin(in_edge, in.d, out.d, currentRequest)
    }

      


      //node.in.map{case (e, i) => println("client %s\n", i.params.)}
    



      

    
      
    

      
      //SynthesizePrintf("Received address %x\n", in.a.bits.address)

    
      //when (in.a.fire)
      //{
      //  SynthesizePrintf("Address: 0x%x\n", in.a.bits.address)
      //}
    

      
    }
    
    //TLArbiter.robin(out_edge, out.a, in.a, manager_in.a)

    //manager_in.d.bits := manager_in_edge.AccessAck(manager_in.a.bits, 0x6969.U)
    //manager_in.d.valid := manager_in_edge.done(manager_in.a)

  

}

trait CanHaveRME extends {
  val rme: Option[RME]
}

trait HasReservedAddressRange extends SimpleDevice {
  hasReservedRange = true
}

trait HasRMEAttachable extends BaseSubsystem {
  val mbus = locateTLBusWrapper(MBUS)
//  mbus.coupleTo("rme-manager") {mbus.rme.get.manager := TLFragmenter(mbus.beatBytes, mbus.blockBytes) := _ }
}