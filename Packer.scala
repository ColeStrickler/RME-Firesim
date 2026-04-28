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




case class PackerColExtractIO(inMaxID:Int, outmaxID : Int, dataRegWidth: Int, minDataSize: Int) extends Bundle {
    val dataIn = Output(UInt(dataRegWidth.W))
    val dataSize = Output(UInt(2.W))
    val placement = Output(UInt(log2Ceil(minDataSize).W))
    val descriptorIn = Output(RequestDescriptor(inMaxID, outmaxID))
}

case class PackerTrapperIO(inMaxID: Int) extends Bundle {
    val PackedLine = Output(UInt(512.W))
    val BaseReqSrc = Output(UInt(log2Ceil(inMaxID).W))
}


class PackerRME(params : RelMemParams, inMaxID: Int, outmaxID : Int) extends Module {


    val beatWidth = 8
    val dataRegWidth = (math.pow(2, params.maxDataSize+1)).toInt * beatWidth // this should give us the extra byte we need to extract excesses
    val io = IO(new Bundle {
        val ColExtractor = Flipped(DecoupledIO(PackerColExtractIO(inMaxID, outmaxID, dataRegWidth, 4)))
        val Trapper = DecoupledIO(PackerTrapperIO(inMaxID))
    })

    when (io.ColExtractor.fire) {
      //  SynthesizePrintf("[Packer] in.fire! %d baseID %d\n", io.ColExtractor.bits.dataSize, io.ColExtractor.bits.descriptorIn.baseID)
    }



    val baseReqSrc = RegInit(0.U(log2Ceil(inMaxID).W))
    val writeBytes = Wire(Vec(8, UInt(8.W))) // max 8 bytes

    for (i <- 0 until 8) {
        writeBytes(i) := io.ColExtractor.bits.dataIn(
            dataRegWidth-1 - (i*8),
            dataRegWidth-8 - (i*8)
        )
    }


    val DataSize = (1.U << io.ColExtractor.bits.dataSize)
    val packedLineBytes = RegInit(VecInit(Seq.fill(64)(0.U(8.W))))
    val byteOffset = io.ColExtractor.bits.descriptorIn.requestPlacement * DataSize
    val NumPackedBytes = RegInit(0.U(7.W))


    when (io.ColExtractor.fire) {
        baseReqSrc := io.ColExtractor.bits.descriptorIn.baseID

        switch(DataSize) {
            is (1.U) {
                packedLineBytes(byteOffset) := writeBytes(0)
                NumPackedBytes := NumPackedBytes + 1.U
            }
            is (2.U) {
                for (i <- 0 until 2) {
                    packedLineBytes(byteOffset + i.U) := writeBytes(i)
                }
                NumPackedBytes := NumPackedBytes + 2.U
            }
            is (4.U) {
                for (i <- 0 until 4) {
                    packedLineBytes(byteOffset + i.U) := writeBytes(i)
                }
                NumPackedBytes := NumPackedBytes + 4.U
            }
            is (8.U) {
                for (i <- 0 until 8) {
                    packedLineBytes(byteOffset + i.U) := writeBytes(i)
                }
                NumPackedBytes := NumPackedBytes + 8.U
            }
        }

        SynthesizePrintf("NumPackedBytes %d\n", NumPackedBytes)
    }



    when ((NumPackedBytes === 64.U))
    {
        SynthesizePrintf("[PACKER FULL]\n")
    }

    //val active :: clear :: Nil = Enum(2)
    //val stateReg = RegInit(active)
    io.ColExtractor.ready := NumPackedBytes =/= 64.U

    // willOverFlow gets set when the next value would overflow the cacheline
    io.Trapper.valid := (NumPackedBytes === 64.U)  //|| willOverflow
    io.Trapper.bits.PackedLine := packedLineBytes.asUInt
    io.Trapper.bits.BaseReqSrc := baseReqSrc

    when (io.Trapper.fire)
    {
        NumPackedBytes := 0.U  
    }
}