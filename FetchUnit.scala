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




class FetchUnitRME(params: RelMemParams, RMEDevice : Device, tlOutEdge: TLEdge, tlOutBundle: TLBundle)(
    implicit p: Parameters) extends LazyModule {
    val io = IO(new Bundle {
        val FetchAddr = Flipped(DecoupledIO(UInt(64.W))) // Receive address to request from the Requestor Module
        val WriteAddrSPM = Flipped(DecoupledIO(UInt(log2Ceil(params.ScratchPadMemSize).W))) // where to write received data to

        val OutputDone = Output(Bool()) // output done tick. Signal so we can start sending back
    })
    val tlOutParams = tlOutEdge.bundle
    val tlOutBeats = tlOutEdge.numBeats(tlOutBundle.a.bits)

    def MakeValidAChannelReq(addr : UInt, isWrite : Bool) : TLBundleA = {
        val req = new TLBundleA(tlOutParams)
        req.address := addr
        req.opcode := Mux(isWrite, TLMessages.PutFullData, TLMessages.Get)
        //req.source
        //req.size
        //req.data
        //req.param


        req
    }


    /*
        We need to be able to manage 1 request at a time which will entail
        taking in a base address and communicating the with Requestor to get the Request Addresses

    */

    lazy val module = new Impl
    class Impl extends LazyModuleImp(this) {

        val currentlyBeating = RegInit(false.B)
        val currentRequest = Wire(Decoupled(new TLBundleA(tlOutParams)))
        val (a_first, a_last, a_done) = tlOutEdge.firstlast(currentRequest)
        currentlyBeating := Mux(currentlyBeating, !a_last, io.FetchAddr.fire)
        io.FetchAddr.ready := a_done || !currentlyBeating

        when (io.FetchAddr.fire)
        {
            // form currentRequest and start sending
        }

    }
}