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












class ControlUnitRME(params: RelMemParams, tlOutEdge: TLEdge, tlOutBundle: TLBundle)(
    implicit p: Parameters) extends LazyModule {
    val io = IO(new Bundle{
        // Config Port 
        val config = Input(RMEConfigPortIO())



        
    })


    lazy val module = new Impl
    class Impl extends LazyModuleImp(this) {



    }

}