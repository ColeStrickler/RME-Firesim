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
import freechips.rocketchip.util.SeqToAugmentedSeq




case class PackerColExtractIO(inMaxID:Int, outmaxID : Int, dataRegWidth: Int, minDataSize: Int, nExtractDesc : Int) extends Bundle {
    //val dataIn = Output(UInt(dataRegWidth.W))
    val dataSize = Output(UInt(2.W))
    //val placement = Output(UInt(log2Ceil(64).W))
    val descriptorIn = Output(RequestDescriptor(inMaxID, outmaxID))

    val dataInValid = Output(Vec(nExtractDesc, Bool()))
    val dataVecIn = Output(Vec(nExtractDesc, UInt(64.W))) 
}

case class PackerTrapperIO(inMaxID: Int) extends Bundle {
    val PackedLine = Output(UInt(512.W))
    val BaseReqSrc = Output(UInt(log2Ceil(inMaxID).W))
}


class PackerRME(params : RelMemParams, inMaxID: Int, outmaxID : Int, nExtractionDesc: Int = 16) extends Module {


    val beatWidth = 8
    val dataRegWidth = (math.pow(2, params.maxDataSize+1)).toInt * beatWidth // this should give us the extra byte we need to extract excesses
    val io = IO(new Bundle {
        val ColExtractor = Flipped(DecoupledIO(PackerColExtractIO(inMaxID, outmaxID, dataRegWidth, 4, nExtractionDesc)))
        val Trapper = DecoupledIO(PackerTrapperIO(inMaxID))
    })

    when (io.ColExtractor.fire) {
      //  SynthesizePrintf("[Packer] in.fire! %d baseID %d\n", io.ColExtractor.bits.dataSize, io.ColExtractor.bits.descriptorIn.baseID)
    }




    val baseReqSrc = RegInit(0.U(log2Ceil(inMaxID).W))
   // val writeBytes = Wire(Vec(8, UInt(8.W))) // max 8 bytes

    //for (i <- 0 until 8) {
    //    writeBytes(i) := io.ColExtractor.bits.dataIn(8*i + 7, 8*i)
    //}


    
    val packedLineBytes = RegInit(VecInit(Seq.fill(64)(0.U(8.W))))

    val NumPackedBytes = RegInit(0.U(7.W))

    val bytesIn = PopCount(io.ColExtractor.bits.dataInValid) << io.ColExtractor.bits.dataSize
   when (io.ColExtractor.fire) {
    baseReqSrc := io.ColExtractor.bits.descriptorIn.baseID
    val DataSize =  io.ColExtractor.bits.dataSize

    SynthesizePrintf("DataSize %d -- %d -- %d\n", DataSize,  bytesIn,  io.ColExtractor.bits.dataInValid.asUInt)
    (0 until nExtractionDesc).foreach{j =>
        val writeBytes = Wire(Vec(8, UInt(8.W))) // max 8 bytes
        for (i <- 0 until 8) {
            writeBytes(i) := io.ColExtractor.bits.dataVecIn(j)(8*i + 7, 8*i)
        }

        val entryValid = io.ColExtractor.bits.dataInValid(j)

        switch (DataSize)
        {
            /* This will straight up not work for 1 and 2. 
            
                We cannot provide a straightforward map from descriptors to placement.
                We do not have enough room.
            */
            is (0.U) 
            {
                assert(false.B)
                //val byteOffset = j
                //when (entryValid)
                //{
                //    for (i <- 0 until 1) {
                //    packedLineBytes((byteOffset + i).U) := writeBytes(i)   
                //    }
                //}

            }
            is (1.U)
            {
                assert(false.B)
                //val byteOffset = j*2
                //when (entryValid) {
                //    for (i <- 0 until 2) {
                //        packedLineBytes((byteOffset + i).U) := writeBytes(i)   
                //    }
                //}
            }
            is (2.U)
            {
                val byteOffset = j*4
                when (entryValid) {
                    for (i <- 0 until 4) {
                        packedLineBytes(byteOffset + i) := writeBytes(i)   
                    }
                }

                SynthesizePrintf("[Packer%d] WriteBytes %d\n", j.U, writeBytes.asUInt)
            }
            is (3.U)
            {
                val byteOffset = j * 8
                if (byteOffset < 64) {
                    when (entryValid) {
                        for (i <- 0 until 8) {
                            packedLineBytes(byteOffset + i) := writeBytes(i)   
                        }
                    }
                }
            }
        }
    }
    //SynthesizePrintf(
    //  "[PackerDataIn] ByteOffset %d Extracted: %d\n",
    //  byteOffset,
    //  io.ColExtractor.bits.dataIn
    //)
    //SynthesizePrintf(
    //  "[Packer]: NumPackedBytes %d\n[Packer]:Line: 0x%x\n",
    //  NumPackedBytes,
    //  packedLineBytes.asUInt
    //)
    }

    NumPackedBytes := Mux(io.Trapper.fire, 0.U, Mux(io.ColExtractor.fire, NumPackedBytes + bytesIn, NumPackedBytes))






    when ((NumPackedBytes > 0.U))
    {
        SynthesizePrintf("NumPacked %d\n", NumPackedBytes)
    }

    //val active :: clear :: Nil = Enum(2)
    //val stateReg = RegInit(active)
    io.ColExtractor.ready := NumPackedBytes =/= 64.U

    // willOverFlow gets set when the next value would overflow the cacheline
    io.Trapper.valid := (NumPackedBytes === 64.U)  //|| willOverflow
    io.Trapper.bits.PackedLine := packedLineBytes.asUInt
    io.Trapper.bits.BaseReqSrc := baseReqSrc
}