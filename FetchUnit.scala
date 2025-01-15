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



case class FetchUnitControlPort() extends Bundle
{
    val data = Output(UInt(512.W)) // 64 bytes = 1 cache line
    val baseAddr = Output(UInt(64.W)) // base address of the request
}





class FetchUnitRME(params: RelMemParams, tlOutEdge: TLEdge, tlOutBundle: TLBundle)(
    implicit p: Parameters) extends LazyModule {

    val tlOutParams = tlOutEdge.bundle
    val tlOutBeats = tlOutEdge.numBeats(tlOutBundle.a.bits)
    val io = IO(new Bundle {
        // Requestor Port
        val Requestor = Flipped(RequestorFetchUnitPort(tlOutParams)) // Receive address to request from the Requestor Module]
        /*
            We will probable want to tag this A channel request on as metadata so we can easily form
            D channel replies to cache
        */

        // DRAM Port
        val OutReq = DecoupledIO(new TLBundleA(tlOutParams)) // send outbound memory requests to DRAM
        val inReply = Flipped(DecoupledIO(new TLBundleD(tlOutParams))) // receive inbound data from DRAM

        
        // Control Unit Port
        val outSPM = DecoupledIO(FetchUnitControlPort())


        // Trapper port --> don't think we need this
        //val OutputDone = Output(Bool()) // output done tick. Signal so we can start sending back
    })
    

    


    /*
        Let's start by being able to manage 1 request at a time which will entail
        taking in a base address and communicating the with Requestor to get the Request Addresses

    */

    lazy val module = new Impl
    class Impl extends LazyModuleImp(this) {


        /*
            [ DRAM OUTBOUND ]
            Handle outbound requests to DRAM
        */
        // Store Current Request in a register to keep its state, pass beating request out in Wire
        val currentlyBeating = RegInit(false.B)
        val currentRequest = Reg(new TLBundleA(tlOutParams))
        val beatingRequest = Wire(Decoupled(new TLBundleA(tlOutParams)))

        
        val (a_first, a_last, a_done) = tlOutEdge.firstlast(beatingRequest)
        currentlyBeating := Mux(currentlyBeating, !a_last, io.Requestor.FetchReq.fire)
        io.Requestor.FetchReq.ready := a_done || !currentlyBeating
        currentRequest := Mux(io.Requestor.FetchReq.fire, io.Requestor.FetchReq.bits, currentRequest)

        beatingRequest.bits := currentRequest
        beatingRequest.valid := currentlyBeating // if we are currently beating request will be valid
        io.OutReq <> beatingRequest



        /*
            [ DRAM INBOUND ]
            Handle Inbound replies from DRAM
        */
        val (d_first, d_last, d_done) = tlOutEdge.firstlast(tlOutBundle.d)
        //val dataRegWriteIndex = RegInit(0.U(log2Ceil(64).W)) //  index for each byte
        val dataReg = RegInit(0.U(512.W)) // store a single cache line we get from DRAM
        val dataRegFull = RegInit(false.B)

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
        dataRegFull := Mux(d_last, true.B, Mux(dataRegFull, !io.outSPM.fire, false.B))
        io.outSPM.valid := dataRegFull // we can write valid data to SPM after receiving entire cache line
        
        

    }
}