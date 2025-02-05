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



case class FetchUnitControlPort(tlParams : TLBundleParameters, maxID : Int) extends Bundle
{
    val data = Output(UInt(512.W)) // 64 bytes = 1 cache line
    val baseReq = Output(new TLBundleA(tlParams))
    val descriptor = Output(new RequestDescriptor(maxID))
}



/* 
    I think we can have several of these, and overlap their latency



    We will also need the incoming requests back to the RME somehow, otherwise we may get a reply that is meant for normal memory
    
*/

class FetchUnitRME(params: RelMemParams, adapter: TLAdapterNode, tlInEdge: TLEdgeIn, instance: Int, subInstance: Int)(
    implicit p: Parameters) extends Module {

    val (out, tlOutEdge) = adapter.out(instance)
    val tlOutA = out.a
    val tlOutD = out.d
    val tlOutParams = tlOutEdge.bundle
    val maxID = (math.pow(2, tlOutParams.sourceBits)-1).toInt
    val io = IO(new Bundle {
        // Requestor Port
        val Requestor = Flipped(Decoupled(new RequestorFetchUnitPort(tlInEdge.bundle, maxID))) // Receive address to request from the Requestor Module]
        //val FetchReq = Flipped(Decoupled(Output(new TLBundleA(tlInEdge.bundle))))
        //val isBaseRequest = Flipped(Output(Bool()))
        //val Requestor_isBaseRequest = Flipped(Decoupled(Bool()))
        //val Requestor_FetchReq = Flipped(Decoupled(new TLBundleA(tlInEdge.bundle)))


        /*
            We will probable want to tag this A channel request on as metadata so we can easily form
            D channel replies to cache
        */

        // DRAM Port
        val OutReq = Decoupled(new TLBundleA(tlOutParams)) // send outbound memory requests to DRAM
        val inReply = Flipped(Decoupled(new TLBundleD(tlOutParams))) // receive inbound data from DRAM
        val SrcId = Valid(UInt(tlOutParams.sourceBits.W)) // use to route incoming requests back to here
        
        
        // Control Unit Port
        val ControlUnit = Decoupled(FetchUnitControlPort(tlOutParams, maxID))


        // Trapper port --> don't think we need this
        //val OutputDone = Output(Bool()) // output done tick. Signal so we can start sending back
    }).suggestName(s"fetchunitio_$instance-$subInstance")


        val baseReq = Reg(new TLBundleA(tlOutParams))
        val descriptor = Reg(new RequestDescriptor(maxID))
        baseReq := Mux(io.Requestor.bits.isBaseRequest && io.Requestor.fire, io.Requestor.bits.FetchReq, baseReq)
        descriptor := Mux(io.Requestor.fire, io.Requestor.bits.descriptor, descriptor)
        when(io.OutReq.fire)
        {
            SynthesizePrintf("[FetchUnit] ==> fired request to DRAM src: %d\n", io.OutReq.bits.source)
        }

        when (io.inReply.fire)
        {
            //SynthesizePrintf("[FetchUnit] ==> received reply DRAM\n")
        }





        /*

            [ DRAM OUTBOUND ]
            Handle outbound requests to DRAM

        */
        // Store Current Request in a register to keep its state, pass beating request out in Wire
        // we may not need to store this in a register here -->?
        val hasActiveRequest = RegInit(false.B)
        val currentlyBeating = RegInit(false.B)
        val currentRequest = Reg(new TLBundleA(tlOutParams))
        val beatingRequest = Wire(Decoupled(new TLBundleA(tlOutParams)))
        val currentBaseAddr = RegInit(0.U(64.W))
        val (a_first, a_last, a_done) = tlOutEdge.firstlast(beatingRequest)
        currentlyBeating := Mux(currentlyBeating, !a_done, io.Requestor.fire)
        currentRequest := Mux(io.Requestor.fire, io.Requestor.bits.FetchReq, currentRequest)
        beatingRequest.bits := currentRequest
        beatingRequest.valid := currentlyBeating
        io.Requestor.ready := !currentlyBeating && !hasActiveRequest

        io.OutReq <> beatingRequest
        

        when (currentRequest.address ===  0x110a71300L.U)
        {
            SynthesizePrintf("addr 0x110a71300. %d, io.OutReq.ready %d\n", currentlyBeating, io.OutReq.ready)
        }



        /*

            [ DRAM INBOUND ]
            Handle Inbound replies from DRAM

        */
        val (d_first, d_last, d_done, _, d_count) = tlOutEdge.firstlast2(io.inReply)

        val dataReg = RegInit(0.U(512.W)) // store a single cache line we get from DRAM
        val dataRegFull = RegInit(false.B)
        val receivedAllData = RegInit(true.B)
        val corrupt = RegInit(false.B)

        io.inReply.ready := !dataRegFull   // can not receive more replies until we have done something with current data
        // shift in new data
        val dataWidth = io.inReply.bits.data.getWidth
        val shiftNewData = io.inReply.bits.data //+ d_count // count is to test
        
        // we have to splice the data after shift because zeroes are put in the top
        dataReg := Mux(io.inReply.fire, Cat(shiftNewData, (dataReg >> dataWidth)(511-dataWidth, 0)), dataReg)
        when (io.inReply.fire)
        {
            SynthesizePrintf("dataReg 0x%x, io.inReply.bits.data 0x%x\n", dataReg, io.inReply.bits.data)
        }

        /*
            if (done receiving data)
                dataRegFull = true
            else
                if (data reg is already full)
                    if we write data to SPM dataRegFull = false
                else
                    dataReg is not full and we stay false
        */
        //SynthesizePrintf("TLBundleD inReply d_first %d, d_last %d, d_done %d, d_count %d, numbeats %d\n", d_first, d_last, d_done, d_count, tlOutEdge.numBeats1(io.inReply.bits))
        dataRegFull := Mux(d_done, true.B, Mux(dataRegFull, !io.ControlUnit.fire, false.B))
        io.ControlUnit.valid := dataRegFull // we can write valid data to SPM after receiving entire cache line
        io.ControlUnit.bits.baseReq := baseReq // will be used to formulate reply
        io.ControlUnit.bits.data := dataReg
        io.ControlUnit.bits.descriptor := descriptor

        // we no longer have an active request when we send it to control unit
        hasActiveRequest := Mux(hasActiveRequest, !io.ControlUnit.fire, io.Requestor.fire) // This is mapped the the io.SrcId.valid, was causing issues in routing the inbound requests
        io.SrcId.bits := currentRequest.source
        io.SrcId.valid := hasActiveRequest
}