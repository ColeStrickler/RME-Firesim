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



case class RMEConfigPortIO(params: RelMemParams) extends Bundle
{
    val RowSize = Output(UInt(32.W)) // size of each row in database
    val RowCount = Output(UInt(32.W)) // count of each row in database
    val EnabledColumnCount = Output(UInt(4.W)) // total number of enabled columns
    val ColumnWidths = Output(UInt(7.W)) // width of ith enabled column
    val ColumnOffsets = Output(Vec(15, UInt(7.W))) // offset off column j from column j-1
    val FrameOffset = Output(UInt(32.W))
    val Enabled = Output(Bool())
    val EphemeralRegionConfig_Start = Output(Vec(params.maxConfigs, UInt(33.W))) 
    val EphemeralRegionConfig_Size = Output(Vec(params.maxConfigs, UInt(log2Ceil(params.rmeAddressSize).W))) 
    val EphemeralRegionConfig_PhysStart = Output(Vec(params.maxConfigs, UInt(47.W)))
}



class ConfigurationPortRME(params: RelMemParams, RMEDevice : Device, instance: Int)(implicit p: Parameters)
{
    



 
    

}