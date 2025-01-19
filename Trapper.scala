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
import _root_.subsystem.rme.subsystem.rme.ConditionalDemuxA






class TrapperRME(params: RelMemParams, tlInEdge: TLEdgeIn, tlInBundle: TLBundle)(
    implicit p: Parameters) extends LazyModule {
    val tlInParams = tlInEdge.bundle
    val tlInBeats = tlInEdge.numBeats(tlInBundle.a.bits)
    val io = IO(new Bundle {
        val TLInA = Flipped(DecoupledIO(new TLBundleA(tlInParams)))
        val TLInD = DecoupledIO(new TLBundleD(tlInParams))
        val TLPassThroughOut = Flipped(DecoupledIO(new TLBundleA(tlInParams)))
        



        val Requestor = RequestorTrapperPort(tlInParams)
        val ControlUnit = Flipped(DecoupledIO(ControlUnitTrapperPort(tlInParams)))

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
        val demux = Module(new ConditionalDemuxA(tlInParams))
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
        val replyCacheLine = RegInit(0.U(512.W))
        val replyToBaseReq = RegInit(new TLBundleA(tlInParams))

        val DataWidth = tlInParams.dataBits

        replyCacheLine := Mux(io.ControlUnit.fire, io.ControlUnit.bits.cacheLine, replyCacheLine)
        replyToBaseReq := Mux(io.ControlUnit.fire, io.ControlUnit.bits.baseReq, replyToBaseReq)
        io.ControlUnit.ready := !currentlyBeating
        


        println("TLBundleD size bits %d\n", tlInParams.sizeBits)
        val dataChanSize = tlInEdge.size(tlInBundle.d.bits)
        val currentRequest = Wire(new TLBundleD(tlInParams))
        val (d_first, d_last, d_done) = tlInEdge.firstlast(io.TLInD)
        val (_, _, _, beatCount) = tlInEdge.count(io.TLInD)
        val currentlyBeating = RegInit(false.B)
        val toSend = Reg(new TLBundleD(tlInParams))
        val currentDataWire = WireInit(0.U(DataWidth.W))
        currentDataWire := (replyCacheLine >> (DataWidth.U*beatCount))(DataWidth-1, 0) // get data
        


        currentlyBeating := Mux(currentlyBeating, !d_last, io.ControlUnit.fire)
        rme_reply_queue.io.deq.ready := !currentlyBeating // && request is ready



        toSend := Mux(io.ControlUnit.fire, tlInEdge.AccessAck(replyToBaseReq, 0.U), toSend)

        currentRequest <> toSend
        io.TLInD.valid := currentlyBeating
        io.TLInD.bits <> currentRequest

      //currentlyBeating := d_first || (currentlyBeating && (beatCounter =/= inDBeats)) 
      //rme_reply_queue.io.enq.valid := rme_in_queue.io.deq.valid 
      //rme_in_queue.io.deq.ready := rme_reply_queue.io.enq.ready
      //val dReply = in_edge.AccessAck(rme_in_queue.io.deq.bits, 0x6969.U)
      //println("dReply.size: %d\n", dReply.size)  
      //rme_reply_queue.io.enq.bits := dReply



    }




}