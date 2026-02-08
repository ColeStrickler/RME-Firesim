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




case class PackerColExtractIO(inMaxID:Int, outmaxID : Int, dataRegWidth: Int) extends Bundle {
    val dataIn = Output(UInt(dataRegWidth.W))
    val dataSize = Output(UInt(10.W))
    val descriptorIn = Output(RequestDescriptor(inMaxID, outmaxID))
}


class PackerRME(params : RelMemParams, inMaxID:Int, outmaxID : Int) extends Module {


    val beatWidth = 8
    val dataRegWidth = (math.pow(2, params.maxDataSize+1)).toInt * beatWidth // this should give us the extra byte we need to extract excesses
    val io = IO(new Bundle {
        val ColExtractor = Flipped(DecoupledIO(PackerColExtractIO(inMaxID, outmaxID, dataRegWidth)))
        val PackedLine = DecoupledIO(UInt(512.W))
        val nPacked = Output(UInt(7.W))
    })


    //val active :: clear :: Nil = Enum(2)
    //val stateReg = RegInit(active)
    val packedLine = RegInit(0.U(512.W))
    val tmpWire = WireInit(0.U(512.W))
    val dataInSizeBits = io.ColExtractor.bits.dataSize * 8.U

    val NumPackedBytes = RegInit(0.U(7.W))
    io.nPacked := NumPackedBytes
    val DataSize = io.ColExtractor.bits.dataSize
    val newDataIn = io.ColExtractor.fire

    // we keep taking data 
    val dataInBounds = NumPackedBytes + io.ColExtractor.bits.dataSize <= 64.U
    val willOverflow = io.ColExtractor.valid && !dataInBounds 
    val ready = io.ColExtractor.valid && dataInBounds //&& (stateReg === active)
    io.ColExtractor.ready := ready

    when (io.ColExtractor.fire)
    {
        //SynthesizePrintf("[PACKER] --> received extracted column 0x%x, size: %d num packed %d\n", io.ColExtractor.bits.dataIn, io.ColExtractor.bits.dataSize, NumPackedBytes)
    }

    /*
        We will need to handle cases when the data doesn't exactly add up to 64bytes eventually --> actually no, we relax this constraint
    */
    val colWidthBits = DataSize*8.U
    val startBit : UInt = (colWidthBits*io.ColExtractor.bits.descriptorIn.requestPlacement)(11, 0) // double check
    // will need more logic for multi-column descriptors

    println("startBit.getWidth %d\n", startBit.getWidth)
    //val mask = (((1.U(64.W) << colWidthBits) - 1.U) << (startBit))(511, 0)


    
    




    val sizeMaskExt = Wire(UInt(512.W))
    when (DataSize === 8.U) {
    sizeMaskExt := "hFFFFFFFFFFFFFFFF".U(512.W)   // lower 8 bits set
    } .elsewhen (DataSize  === 4.U) {
    sizeMaskExt := "hFFFFFFFF".U(512.W)    // lower 4 bytes set
    } .elsewhen (DataSize  === 2.U) {
    sizeMaskExt := "hFFFF".U(512.W)    // lower 2 bytes set
    } .otherwise { // 1 bit
    sizeMaskExt := "hFF".U(512.W)    // lower 1 bytes set
    }




    val mask = (sizeMaskExt << startBit)(511,0)



    //val mask = Wire(UInt(512.W))
    //mask := 0.U
    //mask(startBit + colWidthBits - 1.U, startBit) := 

    when (newDataIn)
    {
        //SynthesizePrintf("[PACKER] --> current line 0x%x\n", packedLine)
        // valid data sizes
        assert(DataSize === 1.U || DataSize === 2.U || DataSize === 4.U || DataSize === 8.U ||
        DataSize === 16.U || DataSize === 32.U || DataSize === 64.U)
        switch(DataSize)
        {
            is (1.U)
            {
                val extractedData = io.ColExtractor.bits.dataIn(dataRegWidth-1, (dataRegWidth-1)-7) 
                val extendedData = extractedData.pad(512)
                val writeData = (extendedData << startBit)(511, 0)
                packedLine := (packedLine & ~mask) | (writeData & mask) 
                NumPackedBytes := NumPackedBytes + 1.U
            }
            is (2.U)
            {
                val extractedData = io.ColExtractor.bits.dataIn(dataRegWidth-1, (dataRegWidth-1)-15) 
                val extendedData = extractedData.pad(512)
                val writeData = (extendedData << startBit)(511, 0)
                packedLine := (packedLine & ~mask) | (writeData & mask) 
                NumPackedBytes := NumPackedBytes + 2.U
            }
            is(4.U)
            {
                val extractedData = io.ColExtractor.bits.dataIn(dataRegWidth-1, (dataRegWidth-1)-31) 
                val extendedData = extractedData.pad(512)
                val writeData = (extendedData << startBit)(511, 0)
                packedLine := (packedLine & ~mask) | (writeData & mask) 
                NumPackedBytes := NumPackedBytes + 4.U

                SynthesizePrintf("extracted Data: 0x%x\n", extractedData)
                SynthesizePrintf("\nextended Data: 0x%x\n", extendedData)
                SynthesizePrintf("\nwrite Data: 0x%x\n", writeData)
                SynthesizePrintf("Start Bit %d, mask 0x%x, Packed line 0x%x\n", startBit, mask, packedLine)
                SynthesizePrintf("io.ColExtractor.bits.descriptorIn.requestPlacement %d\n", io.ColExtractor.bits.descriptorIn.requestPlacement)
            }
            is (8.U)
            {
                val extractedData = io.ColExtractor.bits.dataIn(dataRegWidth-1, (dataRegWidth-1)-63) 
                val extendedData = extractedData.pad(512)
                val writeData = (extendedData << startBit)(511, 0)
                //SynthesizePrintf("extracted Data: 0x%x\n", extractedData)
                //SynthesizePrintf("\nextended Data: 0x%x\n", extendedData)
                //SynthesizePrintf("\nwrite Data: 0x%x\n", writeData)
                //SynthesizePrintf("Start Bit %d, mask 0x%x, Packed line 0x%x\n", startBit, mask, packedLine)
                //SynthesizePrintf("io.ColExtractor.bits.descriptorIn.requestPlacement %d\n", io.ColExtractor.bits.descriptorIn.requestPlacement)
                packedLine := (packedLine & ~mask) | (writeData & mask) 
                NumPackedBytes := NumPackedBytes + 8.U
            }

           // dont allow these for now
            //is (16.U)
            //{
            //    val extractedData = io.ColExtractor.bits.dataIn(511, 511-127) 
            //    val extendedData = extractedData.pad(512)
            //    val writeData = (extendedData << startBit)(511, 0)
            //    packedLine := (packedLine & ~mask) | (writeData & mask) 
            //    //packedLine := Cat(io.ColExtractor.bits.dataIn(511, 511-127), (packedLine >> (dataInSizeBits))(511-128, 0))
            //    NumPackedBytes := NumPackedBytes + 16.U
            //}
            //is (32.U)
            //{
            //    val extractedData = io.ColExtractor.bits.dataIn(511, 511-255) 
            //    val extendedData = extractedData.pad(512)
            //    val writeData = (extendedData << startBit)(511, 0)
            //    packedLine := (packedLine & ~mask) | (writeData & mask) 
            //    NumPackedBytes := NumPackedBytes + 32.U
            //}
            //is (64.U)
            //{
            //    packedLine := io.ColExtractor.bits.dataIn
            //    NumPackedBytes := NumPackedBytes + 64.U
            //}
        }
    }
    


    // willOverFlow gets set when the next value would overflow the cacheline
    io.PackedLine.valid := (NumPackedBytes === 64.U)  //|| willOverflow
    io.PackedLine.bits := packedLine
        

    when (io.PackedLine.fire)
    {
        NumPackedBytes := 0.U  
        //SynthesizePrintf("io.PackedLine.fire NumPackedBytes %d, willOverflow %d\n", NumPackedBytes, willOverflow)
        //SynthesizePrintf("io.PackedLine.fire, Packed line 0x%x\n", packedLine)
    }


    // actually we don't need to do this.
    //when (stateReg === clear)
    //{
    //    packedLine := 0.U // because we are writing via masking, we must zero the register before we begin taking in new
    //}
    
}