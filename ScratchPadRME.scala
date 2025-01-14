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
import _root_.subsystem.rme.subsystem.rme.ScratchPadIO






class ScratchPadRME(params: RelMemParams) extends Module {

    val cacheLineSize = 64
    val metaDataSize = 32

    val io = IO(new Bundle {
        val metadataSPMIO = ScratchPadIO(params.MetadataSPMSize, 32)
        val dataSPMIO = ScratchPadIO(params.DataSPMSize, 64)
    })


    val MetaDataSPM = Module(new ScratchPadMemBank(params.MetadataSPMSize, metaDataSize))
    val CachelineSPM = Module(new ScratchPadMemBank(params.DataSPMSize, cacheLineSize))


    MetaDataSPM.io <> io.metadataSPMIO
    CachelineSPM.io <> io.dataSPMIO

    
}