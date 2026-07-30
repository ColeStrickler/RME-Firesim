package agu


import chisel3._
//import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLBundleA
import freechips.rocketchip.regmapper._
import freechips.rocketchip
import midas.targetutils.SynthesizePrintf
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import scala.collection.mutable.ArrayBuffer
import subsystem.rme.RequestorAGUPort
import subsystem.rme.PrefetchUnitAGUIO
import mainargs.TokensReader.Constant
import subsystem.rme._









class DerefUnit(params: RelMemParams, tlInEdge : TLEdge, tlOutEdge: TLEdge, tlOutBundle: TLBundle, config: Int)(
    implicit p: Parameters) extends Module
{  
    val mem1 = SyncReadMem(32, UInt(512.W))
    val tag1 = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
    val mem2 = SyncReadMem(32, UInt(512.W))
    val tag2 = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))

    // STATE 


}