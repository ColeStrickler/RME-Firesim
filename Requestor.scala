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
import _root_.subsystem.rme.subsystem.rme.IDAllocator
import scala.annotation.meta.param
import os.stat
import agu.AGUTop
import agu.AGUParams



case class RequestDescriptor(maxID : Int) extends Bundle
{
    val baseID = UInt(log2Ceil(maxID).W)
    val allocID = UInt(log2Ceil(maxID).W)
    val requestPlacement = UInt(7.W) // max of 64 places if we are doing 1 byte at a time selection
    val discardFront = UInt(7.W)
    val discardBack = UInt(7.W)
    val beatCount = UInt(4.W)
}

case class RequestorTrapperPort(params : TLBundleParameters) extends Bundle
{
    val Request = DecoupledIO(Output(new TLBundleA(params))) // Send request to fetch unit 
}


case class RequestorFetchUnitPort(params: TLBundleParameters, maxID: Int) extends Bundle
{
    val FetchReq = Output(new TLBundleA(params))
    val BaseReq = Output(new TLBundleA(params))
    val descriptor = Output(new RequestDescriptor(maxID))
}

case class RequestorAGUPort(bitwidth : Int = 32) extends Bundle
{
    //val doGen = Decoupled(Bool())
    val offsetAddrFromBase = Decoupled(UInt(bitwidth.W))    // input
    val offset = Flipped(Decoupled(UInt(bitwidth.W)))       // output
    val data_size = Output(UInt(8.W))                       // used by agu
}



class RequestorRME(params: RelMemParams, tlInEdge : TLEdge, tlOutEdge: TLEdge, tlOutBundle: TLBundle, instance: Int)(
    implicit p: Parameters) extends Module{
        val tlOutParams = tlOutEdge.bundle
        //val tlOutBeats = tlOutEdge.numBeats(tlOutBundle.a.bits)
        val tlInParams = tlInEdge.bundle
        val maxID = (math.pow(2, tlInParams.sourceBits)-1).toInt

         def divideCeil(a: UInt, b: UInt): UInt = {
            (a + b - 1.U) / b
        }
        


        val io = IO(new Bundle {
            // Fetch Unit Port
            val FetchUnit = Decoupled(new RequestorFetchUnitPort(tlInParams, maxID))
            //val FetchReq = Decoupled(Output(new TLBundleA(tlInParams)))
            //val isBaseRequest = Output(Bool())

            // Control Unit Port
            val ControlUnit = Flipped(Decoupled(ControlUnitRequestorPort(maxID)))

            // Config Port
            val Config = Flipped(RMEConfigPortIO())

            // Trapper Port
            val Trapper = Flipped(RequestorTrapperPort(tlInParams))

            val agu = new RequestorAGUPort()

        }).suggestName(s"requestorio_$instance")

        val CacheLineSize = 64 // cache line size in bytes

        // this isn't entirely flexible, assumes 1 bit source expansion
        val id_allocator = Module(new IDAllocator(math.pow(2, tlInParams.sourceBits-1).toInt, maxID))
        
    


        val DatabaseBaseAddress = params.rmeaddress.U
        /*
            We operate on a single bit state machine:

            Idle => ready to receive incoming requests
            Active => generating new modified requests based on config to send to fetch unit
        */
        val active :: idle :: Nil = Enum(2)
        val stateReg = RegInit(idle)
        val requestQueue = Module(new Queue(new TLBundleA(tlInParams), 16, flow=true))
        val outQueue = Module(new Queue(new RequestorFetchUnitPort(tlInParams, maxID), 64, flow=true)) // prevent stalls
        val baseRequest = Reg(new TLBundleA(tlOutParams))
        val ModifiedRequestsSent = WireInit(true.B) // track if we have sent all the necessary requests
        val readyNextReq = Wire(Bool())




        // This should give us the total size in bytes we need to grab
        val TotalCacheLinesNeeded = RegInit(0.U(8.W))
        val TotalCacheLinesSent = RegInit(0.U(4.W))


        val sumColWidths = (io.Config.ColumnWidths*io.Config.EnabledColumnCount).pad(32)
        val nDescriptors = RegInit(0.U(8.W))
        nDescriptors := 64.U/io.Config.ColumnWidths
        val requestOffset = (requestQueue.io.deq.bits.address - params.rmeaddress.U)(31, 0)
        //row := requestRow
        val nDescriptorsSent = RegInit(0.U(8.W))
        //val nSentForProcessing = RegInit(0.U(8.W))
        val sentAddrAGU = RegInit(false.B)
        
        val col = RegInit(0.U(log2Ceil(512 + 1).W))
        val sumOffset = RegInit(0.U(log2Ceil(512 + 1).W))
        val busWidth = 8.U(7.W)

        /* 
            Defaults
        */
        stateReg := stateReg
        baseRequest := baseRequest
        requestQueue.io.enq <> io.Trapper.Request // queue up requests to prevent stalls
        io.Trapper.Request.ready := requestQueue.io.enq.ready

        io.FetchUnit.valid := false.B // default to false
        io.FetchUnit.bits.FetchReq := baseRequest // default 
        io.FetchUnit.bits.BaseReq := baseRequest
        //io.FetchUnit.bits.FetchReq.size := log2Ceil(16).U // size is log2(opsize)
        io.FetchUnit.bits.descriptor.baseID := baseRequest.source
        io.FetchUnit.bits.descriptor.allocID := id_allocator.io.newID.bits

         // this will need to be handled differently once we have multiple valuable data in a single cache line
        io.FetchUnit.bits.descriptor.requestPlacement := TotalCacheLinesSent // FIX LATER
        io.FetchUnit.bits.descriptor.discardBack := 0.U
        io.FetchUnit.bits.descriptor.discardFront := 0.U
        io.FetchUnit.bits.descriptor.beatCount := 0.U

        
        readyNextReq := stateReg === idle
        requestQueue.io.deq.ready := readyNextReq // start new requests when all of old ones have been sent


        id_allocator.io.retireID.bits := io.ControlUnit.bits.retireID // Control unit will retire IDs
        id_allocator.io.retireID.valid := io.ControlUnit.valid
        io.ControlUnit.ready := id_allocator.io.retireID.ready
        id_allocator.io.newID.ready := false.B


        outQueue.io.enq.bits := 0.U.asTypeOf(new RequestorFetchUnitPort(tlInParams, maxID))
        outQueue.io.enq.valid := false.B

        io.FetchUnit.bits := outQueue.io.deq.bits
        io.FetchUnit.valid := outQueue.io.deq.valid
        outQueue.io.deq.ready := io.FetchUnit.ready
        
        //io.agu.doGen.bits := false.B
        //io.agu.doGen.valid := false.B
        io.agu.offset.ready := false.B
        io.agu.data_size := io.Config.ColumnWidths
        io.agu.offsetAddrFromBase.valid := false.B
        io.agu.offsetAddrFromBase.bits := 0.U
        sentAddrAGU := false.B


        when (requestQueue.io.deq.fire)
        {
            //SynthesizePrintf("sumColWidths %d, en col count %d, requestOffset 0x%x\n", sumColWidths, io.Config.EnabledColumnCount, requestOffset)      
            //SynthesizePrintf("Generating requests for 0x%x State %d\n", requestQueue.io.deq.bits.address, stateReg)
        
        }




        when ((outQueue.io.deq.valid && !io.FetchUnit.ready) || outQueue.io.count > 0.U)
        {
            SynthesizePrintf("[REQUESTOR] valid request cannot be sent to fetch units\n")
        }

        switch(stateReg)
        {
            is (idle)
            {
                nDescriptorsSent := 0.U
               // nSentForProcessing := 0.U
                stateReg := Mux(requestQueue.io.deq.fire, active, idle)
                baseRequest := requestQueue.io.deq.bits
                io.agu.offsetAddrFromBase.valid := false.B
                io.agu.offsetAddrFromBase.bits := 0.U
               // io.agu.doGen.bits := false.B
                sentAddrAGU := false.B // i think we can go directly to onm,
            }
            is (active)
            {
                /*
                    Now since we implement the unroll unit, we only need to fire one time here.
                    The rest of the control will be done inside the AGU
                */
                sentAddrAGU := Mux(sentAddrAGU, true.B, io.agu.offsetAddrFromBase.fire)
                io.agu.offsetAddrFromBase.valid := !sentAddrAGU
                io.agu.offsetAddrFromBase.bits := baseRequest.address-params.rmeaddress.U
                SynthesizePrintf("SentAddrAgu %d\n", sentAddrAGU)





                //io.agu.doGen.valid := (nSentForProcessing < nDescriptors)
                //io.agu.doGen.bits := true.B
                //agu.module.io.doGen.fire
                //agu.module.io.offset.fire
                        
                val last = col === io.Config.EnabledColumnCount - 1.U
                val done = last && outQueue.io.enq.fire
                //SynthesizePrintf("[REQUESTOR] baseRequest.address 0x%x\n", baseRequest.address)
                val P_i_j = io.agu.offset.bits
                val R_i_j = (P_i_j / 8.U(32.W)) * busWidth
                val nBeats = divideCeil((P_i_j % busWidth) + io.Config.ColumnWidths, 8.U(60.W))
                val sizeField = OHToUInt(nBeats * 8.U) // need to check this, this should usually turn out fine with col size < 16
                val discardFront = P_i_j % busWidth
                val busAlignment = ((P_i_j + io.Config.ColumnWidths) % busWidth)
                val discardBack = (R_i_j + nBeats*busWidth - (P_i_j + io.Config.ColumnWidths))//Mux(io.Config.ColumnWidths < 8.U, busWidth - busAlignment, busAlignment) 

                val sendRequest = Wire(Valid(new TLBundleA(tlInParams)))
                sendRequest.bits := baseRequest
                sendRequest.bits.address := R_i_j + params.rmeaddress.U
                sendRequest.bits.size := sizeField
                sendRequest.valid := true.B // i think since we switch states we can always set this valid
                

                id_allocator.io.newID.ready := outQueue.io.enq.ready && io.agu.offset.valid // we should then fire, claim id and advance
                val descriptorOut = Wire(RequestDescriptor(maxID))
                descriptorOut.baseID := baseRequest.source
                descriptorOut.allocID := id_allocator.io.newID.bits
                descriptorOut.requestPlacement := nDescriptorsSent
                descriptorOut.discardFront := discardFront
                descriptorOut.discardBack := discardBack
                descriptorOut.beatCount := nBeats
                assert(nBeats > 0.U && nBeats <= 5.U)

                outQueue.io.enq.bits.FetchReq := sendRequest.bits
                outQueue.io.enq.bits.descriptor := descriptorOut
                outQueue.io.enq.bits.BaseReq := baseRequest
                outQueue.io.enq.valid :=  sendRequest.valid && id_allocator.io.newID.fire && io.agu.offset.fire
                io.agu.offset.ready := sendRequest.valid && id_allocator.io.newID.valid && outQueue.io.enq.ready


                when (io.agu.offset.fire)
                {
                    SynthesizePrintf("AGU.fire 0x%x src=%d\n", io.agu.offset.bits, sendRequest.bits.source)
                }

                when (outQueue.io.enq.fire)
                {
                    assert(baseRequest.address >= params.rmeaddress.U && baseRequest.address <= params.rmeaddress.U + params.rmeAddressSize.U)
                    SynthesizePrintf("outQueue.io.enq.fire %d/%d\n", nDescriptorsSent, nDescriptors)
                    //SynthesizePrintf("[REQUESTOR] size %d, P_i_j %d, R_i_j %d\n", sizeField, P_i_j, R_i_j)
                    SynthesizePrintf("REQUESTOR nBeats %d for baseReq 0x%x\n", nBeats, baseRequest.base.address)
                    SynthesizePrintf("[REQUESTOR] nBeats %d, discardFront %d, discardBack %d\n", nBeats, discardFront, discardBack)
                    SynthesizePrintf("[REQUESTOR] sent %d/%d\n", nDescriptorsSent, nDescriptors)
                    //SynthesizePrintf("[REQUESTOR] nSentForProcessing %d\n", nSentForProcessing)
                }


                //nSentForProcessing := nSentForProcessing + io.agu.doGen.fire
                nDescriptorsSent := nDescriptorsSent + outQueue.io.enq.fire
                sumOffset := Mux(outQueue.io.enq.fire, Mux(last, 0.U, sumOffset + io.Config.ColumnOffsets(col)), sumOffset)

                stateReg := Mux(nDescriptorsSent === nDescriptors - 1.U && outQueue.io.enq.fire, idle, stateReg)
            }
        }
        

}