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
import agu.AGUParams2
import agu.ShiftDivider


case class ReqTicketInfo(reqIndexCount : Int, ticketWidth: Int) extends Bundle
{ 
  val index = UInt(log2Ceil(reqIndexCount).W)
  val ticket = UInt(ticketWidth.W)
  val valid = Bool()
}
case class RequestDescriptor(inMaxID:Int, outmaxID : Int) extends Bundle
{
    val baseID = UInt(log2Ceil(inMaxID).W)
    val requestPlacement = UInt(7.W) // max of 64 places if we are doing 1 byte at a time selection
    val discardFront = UInt(7.W)
    val discardBack = UInt(7.W)
    val beatCount = UInt(4.W)
    val config = UInt(4.W)
    val done  = Bool()
    val zero = Bool()
  //  val ticket = UInt(16.W)
}


case class TrapperReq(params : TLBundleParameters, relmemParams : RelMemParams) extends Bundle
{
    val BaseRequest = Output(new TLBundleA(params))
    val configMatch = Output(UInt(log2Ceil(relmemParams.maxConfigs).W))
   // val ticket = Output(UInt(16.W))
}

// We want to change the entire thing to be wrapped in decoupledIO so that we can put in queue
case class RequestorTrapperPort(params : TLBundleParameters, relmemParams : RelMemParams) extends Bundle
{
   // val Request = DecoupledIO(Output(new TLBundleA(params))) // Send request to fetch unit 
    val trapperReq = DecoupledIO(TrapperReq(params, relmemParams))
}


case class RequestorFetchUnitPort(inParams: TLBundleParameters, outParams: TLBundleParameters, inMaxID:Int, outmaxID : Int) extends Bundle
{
    val FetchReq = Output(new TLBundleA(outParams))
    val BaseReq = Output(new TLBundleA(inParams))
    val descriptor = Output(new RequestDescriptor(inMaxID, outmaxID))
}

case class RequestorAGUPort(bitwidth : Int = 32) extends Bundle
{
    //val doGen = Decoupled(Bool())
    val offsetAddrFromBase = Decoupled(UInt(bitwidth.W))    // input
    val offset = Flipped(Decoupled(UInt(bitwidth.W)))       // output
    val data_size = Output(UInt(6.W))                       // used by agu
    val zero = Input(Bool())
}



class RequestorRME(params: RelMemParams, tlInEdge : TLEdge, tlOutEdge: TLEdge, tlOutBundle: TLBundle, config: Int)(
    implicit p: Parameters) extends Module{
        val tlOutParams = tlOutEdge.bundle
        //val tlOutBeats = tlOutEdge.numBeats(tlOutBundle.a.bits)
        val tlInParams = tlInEdge.bundle
        val outMaxID = (math.pow(2, tlOutParams.sourceBits)-1).toInt
        val inMaxID = (math.pow(2, tlInParams.sourceBits)-1).toInt
        val maxRMEOffsetBitWidth = log2Ceil(params.rmeAddressSize)


         def divideCeil(a: UInt, b: UInt): UInt = {
            (a + b - 1.U) / b
        }
        


        val io = IO(new Bundle {
            // Fetch Unit Port
            val FetchUnit = Decoupled(new RequestorFetchUnitPort(tlInParams, tlOutParams, inMaxID, outMaxID))
            //val done = Output(Bool())
            //val FetchReq = Decoupled(Output(new TLBundleA(tlInParams)))
            //val isBaseRequest = Output(Bool())

            // Control Unit Port
            //val ControlUnit = Flipped(Valid(ControlUnitRequestorPort(tlInParams)))

            // Config Port
            val Config = Flipped(RMEConfigPortIO(params))

            // Trapper Port
            val Trapper = Flipped(RequestorTrapperPort(tlInParams, params))

            val agu = new RequestorAGUPort(maxRMEOffsetBitWidth)

        }).suggestName(s"requestorio_$config")

        val CacheLineSize = 64 // cache line size in bytes

        // this isn't entirely flexible, assumes 1 bit source expansion

        val base_allowed_id = math.pow(2, tlOutParams.sourceBits-1).toInt
        val total_ids = outMaxID - base_allowed_id
        val start_id = base_allowed_id + (total_ids/params.maxConfigs)*config
        val num_config_alloc_id = (total_ids/params.maxConfigs) -1
        //val id_allocator = Module(new IDAllocator(start_id, start_id + num_config_alloc_id))
        
    

        /*
            We operate on a single bit state machine:

            Idle => ready to receive incoming requests
            Active => generating new modified requests based on config to send to fetch unit
        */
        val active :: idle :: Nil = Enum(2)
        val stateReg = RegInit(idle)
        val requestQueue = Module(new Queue(TrapperReq(tlInParams, params), 4, flow=true))
        val outQueue = Module(new Queue(new RequestorFetchUnitPort(tlInParams, tlOutParams, inMaxID, outMaxID), 16, flow=true)) // prevent stalls
        val baseRequest = Reg(new TLBundleA(tlInParams))
        val ModifiedRequestsSent = WireInit(true.B) // track if we have sent all the necessary requests
        val readyNextReq = Wire(Bool())
        val currentTicket = Reg(UInt(16.W))

                /*
            This will give the start of the physical range that we are in
        */
        val config_physStart = io.Config.EphemeralRegionConfig_PhysStart(requestQueue.io.deq.bits.configMatch)

        /*
            The size will be the same for the allocated ephemeral backing and the physical range
        */
        val config_size = io.Config.EphemeralRegionConfig_Size(requestQueue.io.deq.bits.configMatch)

        /*
            We use this config parameter so we can allocate the physical region set aside into different pieces
        */
        val EphemeralRegionConfig_Start = io.Config.EphemeralRegionConfig_Start(requestQueue.io.deq.bits.configMatch)

        val newReqOffset = (requestQueue.io.deq.bits.BaseRequest.address - config_physStart)(31, 0)
        when(requestQueue.io.deq.fire)
        {
            //SynthesizePrintf("NewReqOffset 0x%x = 0x%x - 0x%x\n", newReqOffset, requestQueue.io.deq.bits.BaseRequest.address, config_physStart)
        }


        // This should give us the total size in bytes we need to grab
        val TotalCacheLinesNeeded = RegInit(0.U(8.W))
        val TotalCacheLinesSent = RegInit(0.U(4.W))


        val sumColWidths = (io.Config.ColumnWidths*io.Config.EnabledColumnCount).pad(32)
        val nDescriptors = RegInit(0.U(8.W))
        nDescriptors := 64.U/io.Config.ColumnWidths
        val backingEphemeralRegionStart = RegInit(0.U(33.W))
        val requestOffset = RegInit(0.U(maxRMEOffsetBitWidth.W))
        backingEphemeralRegionStart := Mux(requestQueue.io.deq.fire, EphemeralRegionConfig_Start, backingEphemeralRegionStart)
        requestOffset := Mux(requestQueue.io.deq.fire, newReqOffset, requestOffset)
        when (requestQueue.io.deq.fire)
        {
            //SynthesizePrintf("newReqOffset 0x%x\n", newReqOffset)
        }

        //row := requestRow
        val nDescriptorsSent = RegInit(0.U(8.W))
        //val nSentForProcessing = RegInit(0.U(8.W))
        val sentAddrAGU = RegInit(false.B)
        
        //val col = RegInit(0.U(log2Ceil(512 + 1).W))
        val busWidth = 8.U(7.W)

        /* 
            Defaults
        */
      //  io.done := false.B
        stateReg := stateReg
        baseRequest := baseRequest
        requestQueue.io.enq <> io.Trapper.trapperReq  // queue up requests to prevent stalls
        io.Trapper.trapperReq.ready := requestQueue.io.enq.ready
        //currentTicket := 0.U

        io.FetchUnit.valid := false.B // default to false
        io.FetchUnit.bits.FetchReq := baseRequest // default 
        io.FetchUnit.bits.BaseReq := baseRequest
        //io.FetchUnit.bits.FetchReq.size := log2Ceil(16).U // size is log2(opsize)
        io.FetchUnit.bits.descriptor.baseID := baseRequest.source

         // this will need to be handled differently once we have multiple valuable data in a single cache line
        io.FetchUnit.bits.descriptor.requestPlacement := TotalCacheLinesSent // FIX LATER
        io.FetchUnit.bits.descriptor.discardBack := 0.U
        io.FetchUnit.bits.descriptor.discardFront := 0.U
        io.FetchUnit.bits.descriptor.beatCount := 0.U
        
        readyNextReq := stateReg === idle
        requestQueue.io.deq.ready := readyNextReq // start new requests when all of old ones have been sent


        //id_allocator.io.retireID.bits := io.ControlUnit.bits.retireID // Control unit will retire IDs
        //id_allocator.io.retireID.valid := io.ControlUnit.valid
        //io.ControlUnit.ready := id_allocator.io.retireID.ready
        //id_allocator.io.newID.ready := false.B
        


        outQueue.io.enq.bits := 0.U.asTypeOf(new RequestorFetchUnitPort(tlInParams, tlOutParams, inMaxID, outMaxID))
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
         //   SynthesizePrintf("Generating requests for 0x%x State %d\n", requestQueue.io.deq.bits.BaseRequest.address, stateReg)
        
        }

        when (outQueue.io.deq.valid)
        {
           // SynthesizePrintf("Req for config %d, BaseReq %x,\n", outQueue.io.deq.bits.descriptor.config, outQueue.io.deq.bits.BaseReq.address)
        }




        when ((outQueue.io.deq.valid && !io.FetchUnit.ready) || outQueue.io.count > 0.U)
        {
          //  SynthesizePrintf("[REQUESTOR] valid request cannot be sent to fetch units\n")
        }

       

        switch(stateReg)
        {
            is (idle)
            {
                nDescriptorsSent := 0.U
               // nSentForProcessing := 0.U    
                stateReg := Mux(requestQueue.io.deq.fire, active, idle)
                baseRequest := requestQueue.io.deq.bits.BaseRequest
               // currentTicket := requestQueue.io.deq.bits.ticket
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
                io.agu.offsetAddrFromBase.bits := requestOffset
               // SynthesizePrintf("SentAddrAgu %d\n", sentAddrAGU)





                //io.agu.doGen.valid := (nSentForProcessing < nDescriptors)
                //io.agu.doGen.bits := true.B
                //agu.module.io.doGen.fire
                //agu.module.io.offset.fire
                        
      
                //SynthesizePrintf("[REQUESTOR] baseRequest.address 0x%x\n", baseRequest.address)


                // Adjust discards based on whether front or back half is being packed

                //val P_i_j = io.agu.offset.bits
                //val R_i_j = (P_i_j / 8.U(32.W)) * busWidth
                //val nBeats = divideCeil((P_i_j % busWidth) + io.Config.ColumnWidths, 8.U(60.W))
                //val sizeField = OHToUInt(nBeats * 8.U) // need to check this, this should usually turn out fine with col size < 16
                //val discardFront = P_i_j % busWidth
                //val busAlignment = ((P_i_j + io.Config.ColumnWidths) % busWidth)
                //val discardBack = (R_i_j + nBeats*busWidth - (P_i_j + io.Config.ColumnWidths))//Mux(io.Config.ColumnWidths < 8.U, busWidth - busAlignment, busAlignment) 
                
                val P_i_j = io.agu.offset.bits
                val R_i_j = (P_i_j >> 3) * 8.U

                val discard = P_i_j % busWidth
                val nBeats = divideCeil(((discard)(5,0) + io.Config.ColumnWidths(5,0)), 8.U(6.W))


                val sizeTransaction = WireInit(0.U(io.FetchUnit.bits.FetchReq.size.getWidth.W))
                assert(nBeats < 4.U)
                when (nBeats === 3.U) {
                    sizeTransaction := 5.U
                } .elsewhen (nBeats === 2.U) {
                    sizeTransaction := 4.U
                } .otherwise { // 1 bit
                    sizeTransaction := 3.U
                }





                val sizeField = sizeTransaction // need to check this, this should usually turn out fine with col size < 16
                val discardFront = discard
                val busAlignment = ((P_i_j + io.Config.ColumnWidths) % busWidth)
                val discardBack = (R_i_j + (nBeats << 3) - (P_i_j + io.Config.ColumnWidths)) //Mux(io.Config.ColumnWidths < 8.U, busWidth - busAlignment, busAlignment)





                /*
                    Detection logic for when we have data items that can overlap a cache line
                */
                val isFirst = nDescriptorsSent === 0.U
                val isLast = nDescriptorsSent === nDescriptors - 1.U
                val startAddr = backingEphemeralRegionStart + io.agu.offset.bits
                val endAddr = startAddr + io.Config.ColumnWidths
                val cacheLineEnd = (startAddr & ~(0x3F.U)) + 0x40.U
                val overlapsCacheLine = (isFirst || isLast) && (endAddr >= cacheLineEnd)
                val isFrontHalf = isFirst
                //val discardFrontEffective = Mux(isFrontHalf, discardFront, 0.U)
                //val discardBackEffective  = Mux(isFrontHalf, 0.U, discardBack)
                val done = Wire(Bool())
                done := nDescriptorsSent === nDescriptors - 1.U && outQueue.io.enq.fire


                val sendRequest = Wire(Valid(new TLBundleA(tlOutParams)))
                sendRequest.bits := baseRequest
                sendRequest.bits.address := R_i_j + backingEphemeralRegionStart
                sendRequest.bits.size := sizeField
                sendRequest.valid := true.B // i think since we switch states we can always set this valid
                

               // id_allocator.io.newID.ready := outQueue.io.enq.ready && io.agu.offset.valid // we should then fire, claim id and advance
                val descriptorOut = Wire(RequestDescriptor(inMaxID, outMaxID))
                descriptorOut.baseID := baseRequest.source
                descriptorOut.requestPlacement := nDescriptorsSent
                descriptorOut.discardFront := discardFront
                descriptorOut.discardBack := discardBack
                descriptorOut.beatCount := nBeats
                descriptorOut.config := config.U
                descriptorOut.done := done
                descriptorOut.zero := io.agu.zero
                //descriptorOut.ticket := currentTicket
                assert(nBeats > 0.U && nBeats <= 5.U)

                outQueue.io.enq.bits.FetchReq := sendRequest.bits
                outQueue.io.enq.bits.descriptor := descriptorOut
                outQueue.io.enq.bits.BaseReq := baseRequest
                //outQueue.io.enq.valid :=  sendRequest.valid && id_allocator.io.newID.fire && io.agu.offset.fire
                //io.agu.offset.ready := sendRequest.valid && id_allocator.io.newID.valid && outQueue.io.enq.ready
                outQueue.io.enq.valid :=  sendRequest.valid && io.agu.offset.fire
                io.agu.offset.ready := sendRequest.valid && outQueue.io.enq.ready

                when (io.agu.offset.fire)
                {
                    SynthesizePrintf("AGU.fire 0x%x srcAddr=0x%x config %d\n", io.agu.offset.bits, baseRequest.address, config.U)
                }

                when (outQueue.io.enq.fire)
                {
                    //assert(baseRequest.address >= (params.rmeaddress - params.rmeShift).U && baseRequest.address <= (params.rmeaddress - params.rmeShift + params.rmeAddressSize).U)
                   // SynthesizePrintf("outQueue.io.enq.fire %d/%d\n", nDescriptorsSent, nDescriptors)
                    //SynthesizePrintf("[REQUESTOR] size %d, P_i_j %d, R_i_j %d\n", sizeField, P_i_j, R_i_j)
                    //SynthesizePrintf("REQUESTOR nBeats %d for baseReq 0x%x %d\n", nBeats, baseRequest.address, baseRequest.source)
                    //SynthesizePrintf("[REQUESTOR] nBeats %d, discardFront %d, discardBack %d\n", nBeats, discardFront, discardBack)
                   // SynthesizePrintf("[REQUESTOR] sent %d/%d\n", nDescriptorsSent, nDescriptors)
                    //SynthesizePrintf("[REQUESTOR] nSentForProcessing %d\n", nSentForProcessing)
                }


                //nSentForProcessing := nSentForProcessing + io.agu.doGen.fire
                nDescriptorsSent := nDescriptorsSent + outQueue.io.enq.fire

                stateReg := Mux(nDescriptorsSent === nDescriptors - 1.U && outQueue.io.enq.fire, idle, stateReg)

                //io.done := c
            }
        }
        

}