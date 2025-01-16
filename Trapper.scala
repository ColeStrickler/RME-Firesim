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







class TrapperRME(params: RelMemParams, tlInEdge: TLEdge, tlInBundle: TLBundle)(
    implicit p: Parameters) extends LazyModule {
    val tlInParams = tlInEdge.bundle
    val tlInBeats = tlInEdge.numBeats(tlInBundle.a.bits)
    val io = IO(new Bundle {
        val TLInA = Flipped(DecoupledIO(new TLBundleA(tlInParams)))
        val TLOutD = DecoupledIO(new TLBundleD(tlInParams))
        val TLPassThroughOut = Flipped(DecoupledIO(new TLBundleA(tlInParams)))
        



        val Requestor = RequestorTrapperPort(tlInParams)
        val ControlUnit = Flipped(ControlUnitTrapperPort())

    })
    
    def ToRME(addr : UInt) : Bool = {
        val torme : Bool = addr >= params.rmeaddress.U &&  addr <= (params.rmeaddress.U + 0xfff.U)
        torme
    }

    /*
        We need to take in the A Channel and send requests back on the D channel



    */

    lazy val module = new Impl
    class Impl extends LazyModuleImp(this) {
        val isRMERequest = ToRME(io.TLInA.bits.address)
        val demux = Module(new ConditionalDemux(tlInParams))
        demux.io.dataIn <> io.TLInA
        demux.io.sel := isRMERequest
        io.TLPassThroughOut <> demux.io.outA
        io.Requestor.Request <> demux.io.outB // I think we should also send this to the control unit to store metadata


        //rme_in_queue.io.enq <> demux.io.outB

        // Handle inbound request logic


        /*
        
            We should get replies from the Control Unit
        */

        // Handle reply logic
        val rme_reply_queue = Module(new Queue(new TLBundleD(tlInParams), 16, flow=false))
        val currentRequest = Wire(Decoupled(new TLBundleD(tlInParams)))
        val (d_first, d_last, d_done) = tlInEdge.firstlast(currentRequest)
        val currentlyBeating = RegInit(false.B)
        val toSend = Reg(new TLBundleD(tlInParams))


        currentlyBeating := Mux(currentlyBeating, !d_last, rme_reply_queue.io.deq.fire)
        rme_reply_queue.io.deq.ready := !currentlyBeating // && request is ready
        toSend := Mux(rme_reply_queue.io.deq.fire, rme_reply_queue.io.deq.bits, toSend)

        currentRequest.bits <> toSend
        currentRequest.valid := currentlyBeating
        io.TLOutD <> currentRequest

      //currentlyBeating := d_first || (currentlyBeating && (beatCounter =/= inDBeats)) 
      //rme_reply_queue.io.enq.valid := rme_in_queue.io.deq.valid 
      //rme_in_queue.io.deq.ready := rme_reply_queue.io.enq.ready
      //val dReply = in_edge.AccessAck(rme_in_queue.io.deq.bits, 0x6969.U)
      //println("dReply.size: %d\n", dReply.size)  
      //rme_reply_queue.io.enq.bits := dReply



    }




}