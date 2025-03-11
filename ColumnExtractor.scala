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





case class ColumnExtractorIO(maxID : Int) extends Bundle {
    val CacheLineIn = Flipped(DecoupledIO(UInt(512.W))) // take in an entire cache line
    val DescriptorIn = Input(RequestDescriptor(maxID))
   // val DataSizeOut = Output(UInt(7.W)) // size in bytes
    val Packer = DecoupledIO(PackerColExtractIO(maxID))
    //val Data64Out = DecoupledIO(UInt(64.W))
    //val Data32Out = DecoupledIO(UInt(32.W))
    //val Data16Out = DecoupledIO(UInt(16.W))
    //val Data8Out = DecoupledIO(UInt(8.W))
    //val Data4Out = DecoupledIO(UInt(4.W))
    //val Data2Out = DecoupledIO(UInt(2.W))
    //val Data1Out = DecoupledIO(UInt(8.W))
    //val ConfigIn = Input(RMEConfigPortIO()) // do we want to store this so we do not change mid extract?

    
}




class ColumnExtractor(maxID: Int) extends Module {
    /*
        We will shift in a cache line and extract the needed parts 
    */
    val io = IO(ColumnExtractorIO(maxID))

    val tmpLine = RegInit(0.U(512.W))
    val tmpDescriptor = Reg(new RequestDescriptor(maxID))
    val tmpWire = WireInit(0.U(512.W))
    val hasValidLine = RegInit(false.B)
    val currentOffset = RegInit(0.U(64.W))
    /*
        Every cycle, we will compute the offset of the selected data and then shift it into the PackedBytesReg


        we can probably just iterate over the config, and calculate offsets

    */
    
    /*
        To test in the begginning we are just going to take the first 4 bytes from each line
    */
    tmpWire := tmpLine
    //when (io.CacheLineIn.fire)
    //{
    //    SynthesizePrintf("[ColumnExtractor] --> received cache line in\n")
    //}


    io.CacheLineIn.ready := !hasValidLine
    tmpLine := Mux(io.CacheLineIn.fire, io.CacheLineIn.bits, tmpLine)
    tmpDescriptor := Mux(io.CacheLineIn.fire, io.DescriptorIn, tmpDescriptor)
    


    /*
        Next state
    */
    //val ReadyNewLine = NumPackedBytes >= 64.U

    /*
        We will change this logic
    */
    //hasValidLine := Mux(hasValidLine, !io.Packer.fire || io.CacheLineIn.fire, io.CacheLineIn.fire)
    hasValidLine := io.CacheLineIn.fire
    //when (io.CacheLineIn.fire)
    //{
    //    SynthesizePrintf("[ColumnExtractor] --> cache line in\n")
    //}

    val tmpWire2 = WireInit(0.U(512.W))
    val tmpWire3 = WireInit(0.U(512.W))
    //io.DataSizeOut := 16.U
    val TotalSize = (tmpDescriptor.beatCount*8.U) // (8bytes/beat)
    val DataSize = TotalSize - (tmpDescriptor.discardFront) - (tmpDescriptor.discardBack)
    val StartIndex = tmpDescriptor.discardFront*8.U
    val EndIndex = TotalSize - (tmpDescriptor.discardBack*8.U)
    val ShiftAmount = 512.U - TotalSize
    tmpWire2 := (tmpWire >> ShiftAmount)
    val ExtractedData = (tmpWire2 >> StartIndex) & ((1.U << (EndIndex - StartIndex)) - 1.U)
    // Compute how much to shift left for alignment
    val AlignShift = 512.U - (EndIndex - StartIndex)
    // Left-align the extracted data
    val OutputData = ExtractedData << AlignShift

    when (io.Packer.fire)
    {
        //SynthesizePrintf("[COLUMN EXTRACTOR] DataSize: %d, front %d, back %d\n", DataSize, io.DescriptorIn.discardFront, io.DescriptorIn.discardBack)
        //SynthesizePrintf("[COLUMN EXTRACTOR] ExtractedData: 0x%x\n", tmpWire2)
        //SynthesizePrintf("[COLUMN EXTRACTOR] ExtractedData: 0%x\n", ExtractedData)
        //SynthesizePrintf("[COLUMN EXTRACTOR] OutputData: 0x%x\n", OutputData)
    }

    
   
    // send in correct number bits to packer
    io.Packer.bits.dataIn := OutputData//Cat(tmpWire((16*8)-1, 0), 0.U((512-(16*8)).W))
    io.Packer.bits.dataSize := DataSize//(io.DescriptorIn.beatCount*8.U) - io.DescriptorIn.discardFront - io.DescriptorIn.discardBack
    io.Packer.valid := hasValidLine
    io.Packer.bits.descriptorIn := tmpDescriptor
}