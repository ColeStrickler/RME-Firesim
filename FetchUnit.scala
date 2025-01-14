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



class FetchUnitRequest
{
    val addr = UInt(64.W)
    val opcode = UInt(3.W)
    val source = UInt(2.W)

}



class FetchUnitRME(params: RelMemParams, RMEDevice : Device, tlOutEdge: TLEdge, tlOutBundle: TLBundle)(
    implicit p: Parameters) extends LazyModule {

    val tlOutParams = tlOutEdge.bundle
    val tlOutBeats = tlOutEdge.numBeats(tlOutBundle.a.bits)
    val io = IO(new Bundle {
        // Requestor Port
        val FetchReq = Flipped(DecoupledIO(new TLBundleA(tlOutParams))) // Receive address to request from the Requestor Module
        /*
            We will probable want to tag this A channel request on as metadata so we can easily form
            D channel replies to cache
        */

        // DRAM Port
        val OutReq = DecoupledIO(new TLBundleA(tlOutParams)) // send outbound memory requests to DRAM
        val inReply = Flipped(DecoupledIO(new TLBundleD(tlOutParams))) // receive inbound data from DRAM

        // SPM Port
        val WriteAddrSPM = Flipped(DecoupledIO(UInt(log2Ceil(params.ScratchPadMemSize).W))) // where to write received data to
        //val outData = DecoupledIO(new TLBundleD(tlOutParams))


        // Trapper port
        val OutputDone = Output(Bool()) // output done tick. Signal so we can start sending back
    })
    

    


    /*
        Let's start by being able to manage 1 request at a time which will entail
        taking in a base address and communicating the with Requestor to get the Request Addresses

    */

    lazy val module = new Impl
    class Impl extends LazyModuleImp(this) {


        /*
            Handle outbound requests to DRAM
        */
        // Store Current Request in a register to keep its state, pass beating request out in Wire
        val currentlyBeating = RegInit(false.B)
        val currentRequest = Reg(new TLBundleA(tlOutParams))
        val beatingRequest = Wire(Decoupled(new TLBundleA(tlOutParams)))

        
        val (a_first, a_last, a_done) = tlOutEdge.firstlast(beatingRequest)
        currentlyBeating := Mux(currentlyBeating, !a_last, io.FetchReq.fire)
        io.FetchReq.ready := a_done || !currentlyBeating
        currentRequest := Mux(io.FetchReq.fire, io.FetchReq.bits, currentRequest)

        beatingRequest.bits := currentRequest
        beatingRequest.valid := currentlyBeating // if we are currently beating request will be valid
        io.OutReq <> beatingRequest



        /*
            Handle Inbound replies from DRAM
        */
        val (d_first, d_last, d_done) = tlOutEdge.firstlast(tlOutBundle.d)
        val dataRegWriteIndex = RegInit(0.U(log2Ceil(64).W)) //  index for each byte
        val dataReg = RegInit(0.U(512.W)) // store a single cache line we get from DRAM
        

        when (io.inReply.fire)
        {

            // We shift in the received data
            dataReg := Cat((dataReg << (1 << io.inReply.bits.data.getWidth)), io.inReply.bits.data)
        }




    }
}