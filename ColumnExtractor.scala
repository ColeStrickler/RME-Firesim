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





case class CtrlUnitColExtractorIO(inMaxID: Int, outMaxID: Int, nExtractionDesc : Int = 16) extends Bundle {
    val data = UInt(512.W) // take in an entire cache line
    val position = UInt(log2Ceil(64).W)
    val extractionDescriptors = Vec(nExtractionDesc, new ExtractionDescriptor(4))
    val extractionDescriptorsValid = Vec(nExtractionDesc, Bool())
    val descriptorIn = new RequestDescriptor(inMaxID, outMaxID)
}


case class ColumnExtractorIO(inMaxID:Int, outmaxID : Int, dataRegWidth : Int, nExtractionDesc: Int = 16, minDataSize: Int = 4) extends Bundle {

    //val DescriptorIn = Input(RequestDescriptor(inMaxID, outmaxID))
   // val DataSizeOut = Output(UInt(7.W)) // size in bytes
    val CtrlUnit = Flipped(DecoupledIO(new CtrlUnitColExtractorIO(inMaxID, outmaxID)))
    val Packer = DecoupledIO(PackerColExtractIO(inMaxID, outmaxID, dataRegWidth, minDataSize, nExtractionDesc))

    
}




class ColumnExtractor(params: RelMemParams, inMaxID : Int, outmaxID : Int, nExtractionDesc: Int = 16) extends Module {
    /*
        We will shift in a cache line and extract the needed parts 
    */


    val beatWidth = 8
    val dataRegWidth = (math.pow(2, params.maxDataSize+1)).toInt * beatWidth // this should give us the extra byte we need to extract excesses



    val io = IO(new ColumnExtractorIO(inMaxID, outmaxID, dataRegWidth))
    io.Packer.valid := false.B
    io.Packer.bits := 0.U.asTypeOf(new PackerColExtractIO(inMaxID, outmaxID, dataRegWidth, 4, nExtractionDesc))

    val tmpLine = RegInit(0.U(512.W))
    //val tmpDescriptor = Reg(new RequestDescriptor(inMaxID, outmaxID))
    val tmpWire = WireInit(0.U(dataRegWidth.W))
    val hasValidLine = RegInit(false.B)
    val descriptors = RegInit(
    VecInit(Seq.fill(nExtractionDesc)(
        0.U.asTypeOf(new ExtractionDescriptor(4))
    )))

    val descriptorsValid = RegInit(
        VecInit(Seq.fill(nExtractionDesc)(
        false.B)
    ))


    val descriptorCount = RegInit(0.U(log2Ceil(nExtractionDesc+1).W))
    val descriptor = Reg(new RequestDescriptor(inMaxID, outmaxID))

    when(io.CtrlUnit.fire) {
        tmpLine         := io.CtrlUnit.bits.data
        descriptors     := io.CtrlUnit.bits.extractionDescriptors
        descriptorsValid := io.CtrlUnit.bits.extractionDescriptorsValid
        descriptor      := io.CtrlUnit.bits.descriptorIn
    }
    // We should be able to extract everything at once.
    // And just mask when >= descriptorCount
    //def ActiveDescriptor() : ExtractionDescriptor = {
    //    descriptors(descriptorCount-1.U)
    //}

    when(io.CtrlUnit.fire) {
        SynthesizePrintf(
            "[Colxtractor] CTRL FIRE inputValidMask=0x%x\n",
            io.CtrlUnit.bits.extractionDescriptorsValid.asUInt,
        )

    }

    when(io.Packer.fire) {

        SynthesizePrintf(
        "[ColExtractor] PACKER FIRE regMask=0x%x outputMask=0x%x size=%d\n",
        descriptorsValid.asUInt,
        io.Packer.bits.dataInValid.asUInt,
        io.Packer.bits.dataSize
    )
}


    /*
        We can extract everything in parallel
    */

    val validPacker = RegInit(false.B)
    validPacker :=  Mux(io.CtrlUnit.fire, true.B, Mux(io.Packer.fire, false.B, validPacker))
    io.Packer.valid := validPacker
    io.CtrlUnit.ready := !validPacker
    (0 until nExtractionDesc).foreach { i =>
        val edescriptor = descriptors(i)
        val result = Wire(UInt(64.W)) // max = 8 bytes
        result := 0.U

        val byteOffset = Wire(UInt(7.W))
        byteOffset := edescriptor.start
        val dataSize = descriptor.size

        val shifted = tmpLine >> (byteOffset << 3)




        switch(dataSize) {
            is(0.U) { result := shifted(7, 0) }      // 1 byte
            is(1.U) { result := shifted(15, 0) }     // 2 bytes
            is(2.U) { result := shifted(31, 0) }     // 4 bytes
            is(3.U) { result := shifted(63, 0) }     // 8 bytes
        }

        io.Packer.bits.dataVecIn(i) := result
        io.Packer.bits.dataInValid(i) := descriptorsValid(i)
        when (io.Packer.fire)
        {
            SynthesizePrintf("(ColExtract%d) data 0x%x\n", i.U, result)
        }
    

    }





    io.Packer.bits.dataSize := descriptor.size
        

    io.Packer.bits.descriptorIn  := descriptor
   



    //when (descriptorCount > 0.U) {  //
    //    io.Packer.valid := true.B
    //    val desc = ActiveDescriptor()   //
    //    val result = Wire(UInt(64.W)) // max = 8 bytes
    //    result := 0.U   //
    //    val byteOffset = Wire(UInt(7.W))
    //    byteOffset := desc.start
    //    val dataSize = desc.size    //
    //    val shifted = tmpLine >> (byteOffset << 3)  //
    //    
    //    switch(dataSize) {
    //        is(0.U) { result := shifted(7, 0) }      // 1 byte
    //        is(1.U) { result := shifted(15, 0) }     // 2 bytes
    //        is(2.U) { result := shifted(31, 0) }     // 4 bytes
    //        is(3.U) { result := shifted(63, 0) }     // 8 bytes
    //    }   //
    //   // SynthesizePrintf("[ColExtractor] DescriptorCount %d. start %d Extracted: %d\n", descriptorCount,desc.start, result)
    //    
    //    io.Packer.bits.dataIn := result
    //    io.Packer.valid := true.B
    //    io.Packer.bits.placement := desc.pos
    //    io.Packer.bits.descriptorIn  := descriptor
    //}
}