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



case class FetchUnitControlPort(tlParams : TLBundleParameters) extends Bundle
{
    val data = Output(UInt(512.W)) // 64 bytes = 1 cache line
    val baseReq = Output(new TLBundleA(tlParams))
}



/* 
    I think we can have several of these, and overlap their latency



    We will also need the incoming requests back to the RME somehow, otherwise we may get a reply that is meant for normal memory
    
*/

class FetchUnitRME(params: RelMemParams, adapter: TLAdapterNode, tlInEdge: TLEdgeIn, instance: Int)(
    implicit p: Parameters) extends Module {

    val (out, tlOutEdge) = adapter.out(instance)
    val tlOutA = out.a
    val tlOutD = out.d
    val tlOutParams = tlOutEdge.bundle
    val io = IO(new Bundle {
        // Requestor Port
        //val Requestor = Flipped(Decoupled(new RequestorFetchUnitPort(tlInEdge.bundle))) // Receive address to request from the Requestor Module]
        val FetchReq = Flipped(Decoupled(Output(new TLBundleA(tlInEdge.bundle))))
        val isBaseRequest = Flipped(Output(Bool()))
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
        val ControlUnit = Decoupled(FetchUnitControlPort(tlOutParams))


        // Trapper port --> don't think we need this
        //val OutputDone = Output(Bool()) // output done tick. Signal so we can start sending back
    }).suggestName(s"fetchunitio_$instance")
    println("FETCH UNIT\n\n\n")
    println("FETCH UNIT\n\n\n")
    println("FETCH UNIT\n\n\n")
    //println(io.Requestor.bits.)
    //println(io.Requestor.toString())
    //println(io.Requestor.toString())
    println("FETCH UNIT\n\n\n")
    println("FETCH UNIT\n\n\n")
    println("FETCH UNIT\n\n\n")

    /*
        Let's start by being able to manage 1 request at a time which will entail
        taking in a base address and communicating the with Requestor to get the Request Addresses
    */

    val baseReq = Reg(new TLBundleA(tlOutParams))
        baseReq := Mux(io.isBaseRequest && io.FetchReq.fire, io.FetchReq.bits, baseReq)
        when(io.OutReq.fire)
        {
            SynthesizePrintf("[FetchUnit] ==> fired request to DRAM\n")
        }

        when (io.inReply.fire)
        {
            SynthesizePrintf("[FetchUnit] ==> received reply DRAM\n")
        }

        //SynthesizePrintf("[FetchUnit] ==> io.OutReq.ready %d, io.OutReq.valid %d\n", io.OutReq.ready, io.OutReq.valid)
        //SynthesizePrintf("[FetchUnit] ==> io.inReply.ready %d, io.inReply.valid %d\n", io.inReply.ready, io.inReply.valid)
        /*
            [ DRAM OUTBOUND ]
            Handle outbound requests to DRAM
        */
        // Store Current Request in a register to keep its state, pass beating request out in Wire
        val hasActiveRequest = RegInit(false.B)
        val currentlyBeating = RegInit(false.B)
        val currentRequest = Reg(new TLBundleA(tlOutParams))
        val beatingRequest = Wire(Decoupled(new TLBundleA(tlOutParams)))
        val currentBaseAddr = RegInit(0.U(64.W))
        val (a_first, a_last, a_done) = tlOutEdge.firstlast(beatingRequest)
        currentlyBeating := Mux(currentlyBeating, !a_last, io.FetchReq.fire)
        currentRequest := Mux(io.FetchReq.fire, io.FetchReq.bits, currentRequest)
        beatingRequest.bits := currentRequest
        beatingRequest.valid := currentlyBeating
        io.FetchReq.ready := !currentlyBeating && !hasActiveRequest

        io.OutReq <> beatingRequest


        /*
            [ DRAM INBOUND ]
            Handle Inbound replies from DRAM
        */
        val (d_first, d_last, d_done, d_count) = tlOutEdge.firstlast2(io.inReply)




        println("io.inReply.bits.data.getWidth %d\n", io.inReply.bits.data.getWidth)
        
        //val dataRegWriteIndex = RegInit(0.U(log2Ceil(64).W)) //  index for each byte
        val dataReg = RegInit(0.U(512.W)) // store a single cache line we get from DRAM
        val dataRegFull = RegInit(false.B)
        val receivedAllData = RegInit(true.B)
        val corrupt = RegInit(false.B)

        io.inReply.ready := !dataRegFull   // can not receive more replies until we have done something with current data
        // shift in new data
        dataReg := Mux(io.inReply.fire, Cat((dataReg << io.inReply.bits.data.getWidth), io.inReply.bits.data), dataReg)
        
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

        // we no longer have an active request when we send it to control unit
        hasActiveRequest := Mux(hasActiveRequest, !io.ControlUnit.fire, io.FetchReq.fire) // This is mapped the the io.SrcId.valid, was causing issues in routing the inbound requests
        io.SrcId.bits := currentRequest.source
        io.SrcId.valid := hasActiveRequest
}