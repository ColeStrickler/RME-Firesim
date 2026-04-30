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
    val placement = Output(UInt(log2Ceil(64).W))
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
        writeBytes(i) := io.ColExtractor.bits.dataIn(8*i + 7, 8*i)
    }


    val DataSize = (1.U << io.ColExtractor.bits.dataSize)
    val packedLineBytes = RegInit(VecInit(Seq.fill(64)(0.U(8.W))))
    val byteOffset = io.ColExtractor.bits.placement * DataSize

    val NumPackedBytes = RegInit(0.U(7.W))


   when (io.ColExtractor.fire) {
    baseReqSrc := io.ColExtractor.bits.descriptorIn.baseID

    SynthesizePrintf(
      "[PackerDataIn] ByteOffset %d Extracted: %d\n",
      byteOffset,
      io.ColExtractor.bits.dataIn
    )

    for (i <- 0 until 8) {
        when (i.U < DataSize) {
            packedLineBytes(byteOffset + i.U) := writeBytes(i)
        }
    }
    NumPackedBytes := Mux(io.Trapper.fire, 0.U, Mux(io.ColExtractor.fire, NumPackedBytes + DataSize, NumPackedBytes))


    SynthesizePrintf(
      "[Packer]: NumPackedBytes %d\n[Packer]:Line: 0x%x\n",
      NumPackedBytes,
      packedLineBytes.asUInt
    )
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


}