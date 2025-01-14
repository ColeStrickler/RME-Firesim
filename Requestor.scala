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




case class RequestorTrapperPort() extends Bundle
{

}



class RequestorRME(params: RelMemParams, tlOutEdge: TLEdge, tlOutBundle: TLBundle)(
    implicit p: Parameters) extends LazyModule {
        val tlOutParams = tlOutEdge.bundle
        val tlOutBeats = tlOutEdge.numBeats(tlOutBundle.a.bits)
        val io = IO(new Bundle {
            // Fetch Unit Port
            val FetchReq = DecoupledIO(new TLBundleA(tlOutParams)) // Send request to fetch unit 
            

            // Control Unit Port
            val ControlUnit = Flipped(ControlUnitRequestorPort())



            // Config Port
            val Config = Input(RMEConfigPortIO())



            // Trapper Port
            val Trapper = RequestorTrapperPort()

        })

    lazy val module = new Impl
    class Impl extends LazyModuleImp(this) {

        /*
            We operate on a single bit state machine:

            Idle => ready to receive incoming requests
            Active => generating new modified requests based on config to send to fetch unit
        */


    }
}