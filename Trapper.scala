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
        // 
        val TLInA = DecoupledIO(new TLBundleA(tlInParams))

        val Requestor = Flipped(RequestorTrapperPort())
        val ControlUnit = Flipped(ControlUnitTrapperPort())

    })
    


    /*
        We need to take in the A Channel and send requests back on the D channel



    */

    lazy val module = new Impl
    class Impl extends LazyModuleImp(this) {



    }




}