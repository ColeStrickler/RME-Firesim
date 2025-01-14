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



case class RMEConfigPortIO() extends Bundle
{
    val RowSize = UInt(32.W) // size of each row in database
    val RowCount = UInt(32.W) // count of each row in database
    val EnabledColumnCount = UInt(4.W) // total number of enabled columns
    val ColumnWidths = Vec(15, UInt(6.W)) // width of ith enabled column
    val ColumnOffsets = Vec(15, UInt(6.W)) // offset off column j from column j-1
    val FrameOffset = UInt(32.W)
}



class ConfigurationPortRME(params: RelMemParams, RMEDevice : Device)(implicit p: Parameters) extends LazyModule 
{
    val io = IO(new Bundle {
        val config = Output(RMEConfigPortIO())
    })

    val ctlnode = TLRegisterNode(
        address     = Seq(AddressSet(params.controlMMIOAddress, 0xfff)),
        device      = RMEDevice,
        concurrency = 1, // Only one flush at a time (else need to track who answers)
        beatBytes   = params.controlBeatBytes)


    lazy val module = new Impl
    class Impl extends LazyModuleImp(this) {

        // Registers
        val r_RowSize = RegInit(0.U(32.W))
        val r_RowCount = RegInit(0.U(32.W))
        val r_EnabledColumnCount = RegInit(0.U(4.W))
        val r_ColumnWidths = Seq.fill(15)(RegInit(0.U(6.W)))
        val r_ColumnOffsets = Seq.fill(15)(RegInit(0.U(6.W)))
        val r_FrameOffset = RegInit(0.U(32.W))
        val r_Reset = RegInit(false.B)

        // MMIO Register Mapping
        val mmio_RowSize = Seq((0x0) -> Seq(RegField(r_RowSize.getWidth, r_RowSize, RegFieldDesc("RowSize", "RowSizeRME"))))
        val mmio_RowCount = Seq((0x10) -> Seq(RegField(r_RowCount.getWidth, r_RowCount, RegFieldDesc("RowCount", "RowCountRME"))))
        val mmio_EnabledColumnCount = Seq((0x20) -> Seq(RegField(r_EnabledColumnCount.getWidth, r_EnabledColumnCount, RegFieldDesc("EnabledColumnCount", "EnabledColumnCountRME"))))
        val mmio_ColumnWidths = r_ColumnWidths.zipWithIndex.map {case (reg, i) => 
            (i * 10 + 0x30) -> Seq(RegField(reg.getWidth, reg, RegFieldDesc(s"ColumnWidth${i}", "ColumnWidth")))    
        }
        val mmio_ColumnOffsets = r_ColumnOffsets.zipWithIndex.map {case (reg, i) => 
            (i * 0x10 +  15 * 0x10 + 0x30) -> Seq(RegField(reg.getWidth, reg, RegFieldDesc(s"ColumnOffset${i}", "ColumnOffset")))    
        }
        val mmio_FrameOffset = Seq((30 * 0x10 + 0x30) -> Seq(RegField(r_FrameOffset.getWidth, r_FrameOffset, RegFieldDesc("FrameOffset", "FrameOffset"))))
        val mmio_Reset = Seq((31 * 0x10 + 0x30) -> Seq(RegField(r_Reset.getWidth, r_Reset, RegFieldDesc("RMEReset", "RmeReset"))))
        val mmreg = mmio_RowSize ++ mmio_RowCount ++ mmio_EnabledColumnCount ++ 
                    mmio_ColumnWidths ++ mmio_ColumnOffsets ++ mmio_FrameOffset ++ mmio_Reset
        val regmap = ctlnode.regmap(mmreg: _*)

        // Next State Values

        when (r_Reset) // Synchronous High Reset
        {
            // r_Reset := false.B --> we will make software toggle the reset
            r_RowSize := 0.U
            r_RowCount := 0.U
            r_EnabledColumnCount := 0.U
            r_FrameOffset := 0.U
            for (i <- 0 until 15)
            {
                r_ColumnWidths(i) := 0.U
                r_ColumnOffsets(i) := 0.U
            }
        }



        // Assign IO

        io.config.RowSize := r_RowSize
        io.config.RowCount := r_RowCount
        io.config.EnabledColumnCount := r_EnabledColumnCount
        io.config.FrameOffset := r_FrameOffset
        for (i <- 0 until 15)
        {
             io.config.ColumnWidths(i) := r_ColumnWidths(i)
             io.config.ColumnOffsets(i) := r_ColumnOffsets(i)
        }
    }

}