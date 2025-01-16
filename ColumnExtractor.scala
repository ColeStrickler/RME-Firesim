package subsystem.rme

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





case class ColumnExtractorIO() extends Bundle {
    val CacheLineIn = Flipped(DecoupledIO(UInt(512.W))) // take in an entire cache line 
    val DataOut = DecoupledIO(UInt(512.W)) // ouput
    val DataSizeOut = Output(UInt(7.W)) // size in bytes
    val ConfigIn = Input(RMEConfigPortIO()) // do we want to store this so we do not change mid extract?


}




class ColumnExtractor extends Module {
    /*
        We will shift in a cache line and extract the needed parts 
    */
    val io = IO(ColumnExtractorIO())

    val NumPackedBytes = RegInit(0.U(7.W))
    val CurrentColNum = RegInit(0.U(8.W))
    val tmpLine = RegInit(0.U(512.W))
    val PackedBytesReg = RegInit(0.U(512.W)) // store entire cache line
    val Currentoffset = RegInit(0.U(32.W))
    val currentLineOffset = WireInit(0.U(7.W))
    val currentData = WireInit(0.U(128.W)) // 16bytes
    val hasValidLine = RegInit(false.B)
    
    /*
        Every cycle, we will compute the offset of the selected data and then shift it into the PackedBytesReg
    */
    
    /*
        To test in the begginning we are just going to take the first 4 bytes from each line
    */
    val sliced = PackedBytesReg >> (io.ConfigIn.ColumnWidths*8.U) // right shift in data
    PackedBytesReg := Cat(currentData, sliced)

    NumPackedBytes := Mux(hasValidLine, NumPackedBytes + 16.U, NumPackedBytes)


    /*
        Send data out when we have packed an entire cache line
    */
    io.DataOut.valid    := (NumPackedBytes === 64.U)
    io.DataOut.bits     := PackedBytesReg 
}