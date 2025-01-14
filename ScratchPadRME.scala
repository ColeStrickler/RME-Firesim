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
import _root_.subsystem.rme.subsystem.rme.ScratchPadMemBank






class ScratchPadRME(params: RelMemParams)(
    implicit p: Parameters) extends LazyModule {


    lazy val module = new Impl
    class Impl extends LazyModuleImp(this) {
        val MetaDataSPM = Module(new ScratchPadMemBank(1024, 32))
        val CachelineSPM = Module(new ScratchPadMemBank(1024, 64))

    }
}