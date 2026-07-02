package subsystem.rme

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLBundleA
import freechips.rocketchip.regmapper._
import freechips.rocketchip
import midas.targetutils.SynthesizePrintf
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import freechips.rocketchip.diplomacy.BufferParams.flow
import freechips.rocketchip.tilelink.TLMessages.AccessAck
import freechips.rocketchip.tilelink.TLMessages.AccessAckData
import freechips.rocketchip.diplomacy.{AddressRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.subsystem.{BaseSubsystem, MBUS, Attachable}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.subsystem.Attachable
import _root_.subsystem.rme.subsystem.rme.ConditionalDemuxA





class TrapperRME(params: RelMemParams, tlInEdge: TLEdgeIn, tlOutEdge: TLEdgeOut, tlInBundle: TLBundle, instance: Int)(
    implicit p: Parameters) extends Module {
    val tlInParams = tlInEdge.bundle
    val tlOutParams = tlOutEdge.bundle
    val outMaxID = (math.pow(2, tlOutParams.sourceBits)-1).toInt
    val inMaxID = (math.pow(2, tlInParams.sourceBits)-1).toInt

    def ToRME(addr : UInt) : Bool = {
        val torme : Bool = addr >= params.rmeaddress.U &&  addr <= (params.rmeaddress.U + 0xfff.U)
        torme
    }

    def CheckConfigHit(addr: UInt) : (UInt, UInt) = {
            //val hitIndex = Wire(0.U(log2Ceil(params.maxConfigs).W))
            val hits = (0 until params.maxConfigs).map { i =>
            val start = io.Config.EphemeralRegionConfig_PhysStart(i)
            val size  = io.Config.EphemeralRegionConfig_Size(i)
                //SynthesizePrintf("config check %d addr >= 0x%x && addr <= 0x%x\n", i.U, start, start+size)
            (addr >= start) && (addr < (start + size))
            }
            val numHits = PopCount(VecInit(hits)) // counts how many are true
            //assert(numHits > 0.U, "Address matches less than one ephemeral region!")
            //assert(numHits === 1.U, "Address matches more than one ephemeral region!")
            val hitIndex = Mux(numHits === 0.U, 0.U, PriorityEncoder(hits))
            when (numHits === 0.U)
            {
               // SynthesizePrintf("numHits = 0\n\n")
            }



        (hitIndex, numHits)
    }


    def DescriptorToOutReq(src: UInt) : TLBundleA = {
        val ret = Wire(new TLBundleA(tlInParams))
        ret.opcode := TLMessages.Get
        ret.param := 0.U
        ret.size := 6.U
        ret.source := src
        ret.address := 0.U
        ret.data := 0.U
        ret.mask := Fill(ret.mask.getWidth, 1.U(1.W)) // all valid
        ret.corrupt := false.B
        // safest defaults for structured fields
        ret.user := 0.U.asTypeOf(ret.user)
        ret.echo := 0.U.asTypeOf(ret.echo)
        ret
    }





   // val tlInBeats = tlInEdge.numBeats(tlInBundle.a.bits)
    val io = IO(new Bundle {
        val TLInA = Flipped(DecoupledIO(new TLBundleA(tlInParams)))
        val TLInD = DecoupledIO(new TLBundleD(tlInParams))
        val Config = Flipped(RMEConfigPortIO(params))



        val Requestor = new RequestorTrapperPort(tlInParams, params)
        val ControlUnit = Flipped(DecoupledIO(ControlUnitTrapperPort(tlInParams, inMaxID)))

    }).suggestName(s"trapper_$instance")
    println(s"SourceBits ${io.TLInA.bits.source.getWidth}")    
        
    val ticket_dispenser = RegInit(0.U(16.W))
    ticket_dispenser := Mux(io.TLInA.fire, ticket_dispenser + 1.U, ticket_dispenser)

    


    
    /*
        We need to take in the A Channel and send requests back on the D channel
    */
       val matchedConfig = Wire(UInt(log2Ceil(params.maxConfigs).W))
        matchedConfig := 0.U
        val (mconfig, nhits) =  CheckConfigHit(io.TLInA.bits.address)

        matchedConfig := mconfig
        when (io.TLInA.fire)
        {
            
            SynthesizePrintf("io.TLInA.address 0x%x --> %d ticket: %d --> config %d\n", io.TLInA.bits.address, io.TLInA.bits.source, ticket_dispenser, matchedConfig)         
        }
        

        /*
                    
            val config_physStart = io.Config.EphemeralRegionConfig_PhysStart(matchedConfig)
            val config_size = io.Config.EphemeralRegionConfig_Size(matchedConfig)
            val EphemeralRegionConfig_Start = io.Config.EphemeralRegionConfig_Start(matchedConfig)


            From these we get the offset via --> offset = TLInA.bits.addr - config_physStart


            EphemeralRegionConfig_Start is the base of the data region. We use these so we can have an allocator
            split up the region. With the absolute offset we can calculate the offsets of the data pieces that and add those onto
            EphemeralRegionConfig_Start. 

            we can just simply pass in the matched config to the Requestor
        
        */




       

        io.Requestor.trapperReq.valid := io.TLInA.valid // I think we should also send this to the control unit to store metadata
        io.TLInA.ready := io.Requestor.trapperReq.ready
        io.Requestor.trapperReq.bits.BaseRequest := io.TLInA.bits
        io.Requestor.trapperReq.bits.configMatch := matchedConfig
       // io.Requestor.trapperReq.bits.ticket := ticket_dispenser

        // Handle inbound request logic


        /*
        
            We should get replies from the Control Unit
        */

        // Handle reply logic
       // val rme_reply_queue = Module(new Queue(new TLBundleD(tlInParams), 16, flow=false))
        val replyCacheLine = RegInit(0.U(512.W))
        val replyToBaseReq = Reg(new TLBundleA(tlInParams))

        val DataWidth = tlInParams.dataBits

        
         


        println("TLBundleD size bits %d\n", tlInParams.sizeBits)
        val dataChanSize = tlInEdge.size(tlInBundle.d.bits)
        val currentRequest = Wire(Decoupled(new TLBundleD(tlInParams)))
        val (d_first, d_last, d_done, beatCount, count) = tlInEdge.firstlast2(currentRequest)
        val currentlyBeating = RegInit(false.B)
        val toSend = Reg(new TLBundleD(tlInParams))
        val currentDataWire = WireInit(0.U(DataWidth.W))
        currentDataWire := replyCacheLine(DataWidth-1, 0) // get data
        replyCacheLine := Mux(io.ControlUnit.fire, io.ControlUnit.bits.cacheLine, Mux(io.TLInD.fire, replyCacheLine >> DataWidth, replyCacheLine))
        
        replyToBaseReq := Mux(io.ControlUnit.fire, DescriptorToOutReq(io.ControlUnit.bits.baseReqSource), replyToBaseReq)
        io.ControlUnit.ready := !currentlyBeating


        when (io.ControlUnit.fire)
        {
          //  SynthesizePrintf("[TRAPPER] --> cache line from control unit. \n")
        }

        currentlyBeating := Mux(currentlyBeating, !d_done, io.ControlUnit.fire)


       // since we modify the size to be bus width granularity, we set it back here
        val baseReqUpdated = Wire(new TLBundleA(tlInParams))
        baseReqUpdated := replyToBaseReq
        baseReqUpdated.size := 6.U
        
      
        toSend := Mux(io.ControlUnit.fire, tlInEdge.AccessAck(replyToBaseReq, currentDataWire), toSend)

        currentRequest.bits := tlInEdge.AccessAck(replyToBaseReq, currentDataWire)
        currentRequest.valid := currentlyBeating


        when (io.TLInA.fire) {
          //  SynthesizePrintf("[TRAPPER] --> io.TLInA.fire\n")
        }
        
        when (io.ControlUnit.fire)
        {
         //   SynthesizePrintf("(FromControl) line in 0x%x\n",io.ControlUnit.bits.cacheLine)
        }

        when (io.TLInD.fire && d_first)
        {
           // SynthesizePrintf("[TRAPPER] ==> reply cacheLine: 0x%x\n", replyCacheLine)
            SynthesizePrintf("[TRAPPER] ==> sent reply to 0x%x with data: 0x%x to source %d, size %d\n", baseReqUpdated.address, currentRequest.bits.data, currentRequest.bits.source, currentRequest.bits.size)
        }
        
        io.TLInD <> currentRequest

        //when (currentlyBeating)
        //{
        //    SynthesizePrintf("[TRAPPER] --> currentlyBeating. io.TLInD.ready %d, d_done %d, count %d\n", io.TLInD.ready, d_done, count)
        //}

}