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
    val nDesc = UInt(log2Ceil(nExtractionDesc+1).W)
    val descriptorIn = new RequestDescriptor(inMaxID, outMaxID)
}


case class ColumnExtractorIO(inMaxID:Int, outmaxID : Int, dataRegWidth : Int, nExtractionDesc: Int = 16, minDataSize: Int = 4) extends Bundle {

    //val DescriptorIn = Input(RequestDescriptor(inMaxID, outmaxID))
   // val DataSizeOut = Output(UInt(7.W)) // size in bytes
    val CtrlUnit = Flipped(DecoupledIO(new CtrlUnitColExtractorIO(inMaxID, outmaxID)))
    val Packer = DecoupledIO(PackerColExtractIO(inMaxID, outmaxID, dataRegWidth, minDataSize))

    
}




class ColumnExtractor(params: RelMemParams, inMaxID : Int, outmaxID : Int, nExtractionDesc: Int = 16) extends Module {
    /*
        We will shift in a cache line and extract the needed parts 
    */


    val beatWidth = 8
    val dataRegWidth = (math.pow(2, params.maxDataSize+1)).toInt * beatWidth // this should give us the extra byte we need to extract excesses



    val io = IO(new ColumnExtractorIO(inMaxID, outmaxID, dataRegWidth))
    io.Packer.valid := false.B
    io.Packer.bits := 0.U.asTypeOf(new PackerColExtractIO(inMaxID, outmaxID, dataRegWidth, 4))

    val tmpLine = RegInit(0.U(512.W))
    //val tmpDescriptor = Reg(new RequestDescriptor(inMaxID, outmaxID))
    val tmpWire = WireInit(0.U(dataRegWidth.W))
    val hasValidLine = RegInit(false.B)
    val descriptors = RegInit(
    VecInit(Seq.fill(nExtractionDesc)(
        0.U.asTypeOf(new ExtractionDescriptor(4))
    ))
)
    val descriptorCount = RegInit(0.U(log2Ceil(nExtractionDesc+1).W))
    val descriptor = Reg(new RequestDescriptor(inMaxID, outmaxID))

    tmpLine := Mux(io.CtrlUnit.fire, io.CtrlUnit.bits.data, tmpLine)
    descriptors := Mux(io.CtrlUnit.fire, io.CtrlUnit.bits.extractionDescriptors, descriptors)
    descriptorCount := Mux(io.CtrlUnit.fire, io.CtrlUnit.bits.nDesc, Mux(io.Packer.fire, descriptorCount-1.U, descriptorCount))
    io.CtrlUnit.ready := descriptorCount === 0.U
    descriptor := Mux(io.CtrlUnit.fire, io.CtrlUnit.bits.descriptorIn, descriptor)

    
    def ActiveDescriptor() : ExtractionDescriptor = {
        descriptors(descriptorCount-1.U)
    }

    when (io.CtrlUnit.fire) {
         SynthesizePrintf("[ColumnExtractor] in.fire! descCount 0x%x baseID %d dataIn 0x%x\n", io.CtrlUnit.bits.nDesc, io.CtrlUnit.bits.descriptorIn.baseID, io.CtrlUnit.bits.data)
    }


    when (descriptorCount > 0.U) {

        io.Packer.valid := true.B
        val desc = ActiveDescriptor()

        val result = Wire(UInt(64.W)) // max = 8 bytes
        result := 0.U

        val byteOffset = Wire(UInt(7.W))
        byteOffset := desc.start
        val dataSize = desc.size

        val shifted = tmpLine >> (byteOffset << 3)

        
        switch(dataSize) {
            is(0.U) { result := shifted(7, 0) }      // 1 byte
            is(1.U) { result := shifted(15, 0) }     // 2 bytes
            is(2.U) { result := shifted(31, 0) }     // 4 bytes
            is(3.U) { result := shifted(63, 0) }     // 8 bytes
        }

        SynthesizePrintf("[ColExtractor] DescriptorCount %d. start %d Extracted: %d\n", descriptorCount,desc.start, result)
        io.Packer.bits.dataSize := desc.size
        io.Packer.bits.dataIn := result
        io.Packer.valid := true.B
        io.Packer.bits.placement := desc.pos
        io.Packer.bits.descriptorIn  := descriptor
    }
}