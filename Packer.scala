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
import org.apache.commons.compress.java.util.jar.Pack200.Packer



case class PackerColExtractIO(maxID : Int) extends Bundle {
    val dataIn = Output(UInt(512.W))
    val dataSize = Output(UInt(10.W))
    val descriptorIn = Output(RequestDescriptor(maxID))
}


class PackerRME(maxID: Int) extends Module {

    val io = IO(new Bundle {
        val ColExtractor = Flipped(DecoupledIO(PackerColExtractIO(maxID)))
        val PackedLine = DecoupledIO(UInt(512.W))
    })


    //val active :: clear :: Nil = Enum(2)
    //val stateReg = RegInit(active)
    val packedLine = RegInit(0.U(512.W))
    val tmpWire = WireInit(0.U(512.W))
    val dataInSizeBits = io.ColExtractor.bits.dataSize * 8.U

    val NumPackedBytes = RegInit(0.U(7.W))

    val DataSize = io.ColExtractor.bits.dataSize
    val newDataIn = io.ColExtractor.fire

    // we keep taking data 
    val dataInBounds = NumPackedBytes + io.ColExtractor.bits.dataSize <= 64.U
    val willOverflow = io.ColExtractor.valid && !dataInBounds 
    val ready = io.ColExtractor.valid && dataInBounds //&& (stateReg === active)
    io.ColExtractor.ready := ready

    //when (io.ColExtractor.fire)
    //{
    //    SynthesizePrintf("[PACKER] --> received extracted column 0x%x\n", io.ColExtractor.bits.dataIn)
    //}

    /*
        We will need to handle cases when the data doesn't exactly add up to 64bytes eventually --> actually no, we relax this constraint
    */
    val colWidthBits = 16*8
    val startBit : UInt = colWidthBits.U*io.ColExtractor.bits.descriptorIn.requestPlacement // double check
    // will need more logic for multi-column descriptors


    val mask = ((BigInt(1) << colWidthBits) - 1).U << (startBit)

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
                packedLine := Cat(io.ColExtractor.bits.dataIn(511, 511-7), (packedLine >> (dataInSizeBits))(511-8, 0))
                NumPackedBytes := NumPackedBytes + 1.U
            }
            is (2.U)
            {
                packedLine := Cat(io.ColExtractor.bits.dataIn(511, 511-15), (packedLine >> (dataInSizeBits))(511-16, 0))
                NumPackedBytes := NumPackedBytes + 2.U
            }
            is(4.U)
            {
                packedLine := Cat(io.ColExtractor.bits.dataIn(511, 511-31), (packedLine >> (dataInSizeBits))(511-32, 0))
                NumPackedBytes := NumPackedBytes + 4.U
            }
            is (8.U)
            {
                packedLine := Cat(io.ColExtractor.bits.dataIn(511, 511-63), (packedLine >> (dataInSizeBits))(511-64, 0))
                NumPackedBytes := NumPackedBytes + 8.U
            }
            is (16.U)
            {
                val writeData = io.ColExtractor.bits.dataIn(511, 511-127) << startBit
                packedLine := (packedLine & ~mask) | (writeData & mask) 
                //packedLine := Cat(io.ColExtractor.bits.dataIn(511, 511-127), (packedLine >> (dataInSizeBits))(511-128, 0))
                NumPackedBytes := NumPackedBytes + 16.U
            }
            is (32.U)
            {
                packedLine := Cat(io.ColExtractor.bits.dataIn(511, 511-255), (packedLine >> (dataInSizeBits))(511-256, 0))
                NumPackedBytes := NumPackedBytes + 32.U
            }
            is (64.U)
            {
                packedLine := io.ColExtractor.bits.dataIn
                NumPackedBytes := NumPackedBytes + 64.U
            }
        }
    }
    


    // willOverFlow gets set when the next value would overflow the cacheline
    io.PackedLine.valid := (NumPackedBytes === 64.U)  || willOverflow
    io.PackedLine.bits := packedLine
        

    when (io.PackedLine.fire)
    {
        NumPackedBytes := 0.U  
    }


    // actually we don't need to do this.
    //when (stateReg === clear)
    //{
    //    packedLine := 0.U // because we are writing via masking, we must zero the register before we begin taking in new
    //}
    
}