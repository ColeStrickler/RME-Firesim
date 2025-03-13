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



class RequestorRME(params: RelMemParams, tlInEdge : TLEdge, tlOutEdge: TLEdge, tlOutBundle: TLBundle, instance: Int)(
    implicit p: Parameters) extends Module {
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
        val baseRequest = Reg(new TLBundleA(tlOutParams))
        val ModifiedRequestsSent = WireInit(true.B) // track if we have sent all the necessary requests
        val readyNextReq = Wire(Bool())

        // What happens if enabled column count changes while we're handling request? --> we can probably relax this assumption
        // I think this will lead to issues of incomplete request formation
        // We will load a new config here so it does not get modified mid request generation
        val CurrentRowSize = RegInit(0.U(32.W)) // size of each row in database
        val CurrentRowCount = RegInit(0.U(32.W))  // count of each row in database
        val CurrentEnabledColumnCount = RegInit(0.U(4.W))  // total number of enabled columns
        val CurrentColumnWidth = RegInit(0.U(7.W)) // width of ith enabled column
        val CurrentColumnOffsets =  RegInit(VecInit(Seq.fill(15)(0.U(7.W)))) // offset off column j from column j-1
        val CurrentFrameOffset = RegInit(0.U(32.W))

        // This should give us the total size in bytes we need to grab
        val TotalCacheLinesNeeded = RegInit(0.U(8.W))
        val TotalCacheLinesSent = RegInit(0.U(4.W))


        val sumColWidths = (io.Config.ColumnWidths*io.Config.EnabledColumnCount).pad(32)
        val nDescriptors = RegInit(0.U(8.W))
        nDescriptors := 64.U/io.Config.ColumnWidths
        val requestOffset = (requestQueue.io.deq.bits.address - params.rmeaddress.U)(31, 0)
        val requestRow = requestOffset / sumColWidths//requestOffset / io.Config.RowSize.pad(33)//(requestOffset - (requestOffset % io.Config.RowSize))
        val row = RegInit(0.U(log2Ceil(params.rmeAddressSize).W))
        //row := requestRow
        val nDescriptorsSent = RegInit(0.U(8.W))
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


        when (requestQueue.io.deq.fire)
        {
            //SynthesizePrintf("sumColWidths %d, en col count %d, requestOffset 0x%x\n", sumColWidths, io.Config.EnabledColumnCount, requestOffset)      
            //SynthesizePrintf("Generating requests for 0x%x State %d\n", requestQueue.io.deq.bits.address, stateReg)
        
        }

        switch(stateReg)
        {
            is (idle)
            {
                nDescriptorsSent := 0.U
                col := 0.U
                sumOffset := 0.U
                row := requestRow
                stateReg := Mux(requestQueue.io.deq.fire, active, idle)
                baseRequest := requestQueue.io.deq.bits
            }
            is (active)
            {
                //SynthesizePrintf("Rowsize %d, row %d, io.Config.ColumnOffsets(col) %d\n", io.Config.RowSize, row, io.Config.ColumnOffsets(col))
                
                
                val last = col === io.Config.EnabledColumnCount - 1.U
                val done = last && io.FetchUnit.fire
                val P_i_j = (io.Config.RowSize * row) + (sumOffset + io.Config.ColumnOffsets(col))
                //SynthesizePrintf("[REQUESTOR] baseRequest.address 0x%x\n", baseRequest.address)
                val R_i_j = (P_i_j / 8.U(60.W)) * busWidth
                val nBeats = divideCeil((P_i_j % busWidth) + io.Config.ColumnWidths, 8.U(60.W))
                val sizeField = OHToUInt(nBeats * 8.U) // need to check this, this should usually turn out fine with col size < 16
                val discardFront = P_i_j % busWidth


                /*
                    We altered this from the EDBT paper
   
                    This needs edited -> causing freeze with multi-column 
                */
                val busAlignment = ((P_i_j + io.Config.ColumnWidths) % busWidth)
                val discardBack = (R_i_j + nBeats*busWidth - (P_i_j + io.Config.ColumnWidths))//Mux(io.Config.ColumnWidths < 8.U, busWidth - busAlignment, busAlignment) 

                val sendRequest = Wire(Valid(new TLBundleA(tlInParams)))
                sendRequest.bits := baseRequest
                sendRequest.bits.address := R_i_j + params.rmeaddress.U
                sendRequest.bits.size := sizeField
                sendRequest.valid := true.B // i think since we switch states we can always set this valid
                

                id_allocator.io.newID.ready := io.FetchUnit.ready // we should then fire, claim id and advance
                val descriptorOut = Wire(RequestDescriptor(maxID))
                descriptorOut.baseID := baseRequest.source
                descriptorOut.allocID := id_allocator.io.newID.bits
                descriptorOut.requestPlacement := nDescriptorsSent
                descriptorOut.discardFront := discardFront
                descriptorOut.discardBack := discardBack
                descriptorOut.beatCount := nBeats
                assert(nBeats > 0.U && nBeats <= 5.U);

                io.FetchUnit.bits.FetchReq := sendRequest.bits
                io.FetchUnit.bits.descriptor := descriptorOut
                io.FetchUnit.bits.BaseReq := baseRequest
                io.FetchUnit.valid :=  sendRequest.valid && id_allocator.io.newID.fire
                

                when (io.FetchUnit.fire)
                {
                    assert(baseRequest.address >= params.rmeaddress.U && baseRequest.address <= params.rmeaddress.U + params.rmeAddressSize.U)
                    //SynthesizePrintf("[REQUESTOR] size %d, P_i_j %d, R_i_j %d\n", sizeField, P_i_j, R_i_j)
                    SynthesizePrintf("REQUESTOR nBeats %d for baseReq 0x%x\n", nBeats, baseRequest.base.address)
                    
                    //SynthesizePrintf("[REQUESTOR] nBeats %d, discardFront %d, discardBack %d\n", nBeats, discardFront, discardBack)
                    //SynthesizePrintf("[REQUESTOR] sent %d/%d\n", nDescriptorsSent, nDescriptors)
                }

                nDescriptorsSent := nDescriptorsSent + io.FetchUnit.fire
                sumOffset := Mux(io.FetchUnit.fire, Mux(last, 0.U, sumOffset + io.Config.ColumnOffsets(col)), sumOffset)
                col := Mux(io.FetchUnit.fire, Mux(last, 0.U, col + 1.U), col)
                row := Mux(done, row + 1.U, row)
                stateReg := Mux(nDescriptorsSent === nDescriptors - 1.U && io.FetchUnit.fire, idle, stateReg)
            }
        }
        
        


        //when (io.FetchUnit.fire)
        //{
        //    SynthesizePrintf("[REQUESTOR] ==> Sent request to fetch unit base src: %d, alloc src %d\n", baseRequest.source, id_allocator.io.newID.bits)
        //}
  
        


        /*
        // Next state logic
        switch(stateReg)
        {
            is (idle) {
                // When we have a new request we are now active
                stateReg := Mux(!ModifiedRequestsSent, active, idle)

                // when we receive a new request in, we have no longer sent all requests
                ModifiedRequestsSent := Mux(requestQueue.io.deq.fire, false.B, true.B)
                baseRequest := requestQueue.io.deq.bits


                /*
                    Update config
                */
                CurrentRowSize              := io.Config.RowSize
                CurrentRowCount             := io.Config.RowCount
                CurrentEnabledColumnCount   := io.Config.EnabledColumnCount
                CurrentColumnWidths         := io.Config.ColumnWidths
                CurrentColumnOffsets        := io.Config.ColumnOffsets
                CurrentFrameOffset          := io.Config.FrameOffset
                // Computer how much data we need to fetch to construct a single cache line
                val singleRowEnColSize = (io.Config.ColumnWidths * io.Config.EnabledColumnCount)
                val rowsNeeded = divideCeil(64.U, singleRowEnColSize)
                val cacheLinesNeeded = divideCeil(rowsNeeded*io.Config.RowSize, 64.U(64.W))
                TotalCacheLinesNeeded       := 4.U//cacheLinesNeeded
                TotalCacheLinesSent         := 0.U
                id_allocator.io.newID.ready := false.B

            }
            is (active) {
                /*
                    If we have sent all the necessary requests, we transition back to idle state
                */
                stateReg := Mux(ModifiedRequestsSent, idle, active)

                baseRequest := baseRequest // keep base request for all active states

                /*
                    Send requests to fetch unit
                */
                val sendRequest = Wire(Valid(new TLBundleA(tlInParams)))
                sendRequest.bits := baseRequest
                // cache line size = 64 bytes, so we increment each request by 0x40
                sendRequest.bits.address := baseRequest.address + (TotalCacheLinesSent * 0x40.U)
                sendRequest.valid := true.B && !readyNextReq
                io.FetchUnit.bits.FetchReq := sendRequest.bits
                io.FetchUnit.bits.FetchReq.size := log2Ceil(16).U // size is log2(opsize) --> request at bus width granularity
                io.FetchUnit.valid := sendRequest.valid && id_allocator.io.newID.fire
                id_allocator.io.newID.ready := io.FetchUnit.ready // circular logic?
                io.FetchUnit.bits.isBaseRequest := (TotalCacheLinesSent === 0.U) // first req


                /*
                    Update config before generating next request
                */
                CurrentRowSize              := Mux(ModifiedRequestsSent, io.Config.RowSize, CurrentRowSize)
                CurrentRowCount             := Mux(ModifiedRequestsSent, io.Config.RowCount, CurrentRowCount)
                CurrentEnabledColumnCount   := Mux(ModifiedRequestsSent, io.Config.EnabledColumnCount, CurrentEnabledColumnCount)
                CurrentColumnWidths         := Mux(ModifiedRequestsSent, io.Config.ColumnWidths, CurrentColumnWidths)
                CurrentColumnOffsets        := Mux(ModifiedRequestsSent, io.Config.ColumnOffsets, CurrentColumnOffsets)
                CurrentFrameOffset          := Mux(ModifiedRequestsSent, io.Config.FrameOffset, CurrentFrameOffset)
                // if we have an offset > 64 we can skip a line)
                TotalCacheLinesNeeded       := Mux(ModifiedRequestsSent, // maybe an error?
                    0.U, TotalCacheLinesNeeded) 
                TotalCacheLinesSent         := Mux(!io.FetchUnit.fire, TotalCacheLinesSent,
                    Mux(TotalCacheLinesSent < TotalCacheLinesNeeded - 1.U, TotalCacheLinesSent + 1.U, 0.U))
                ModifiedRequestsSent        := Mux(TotalCacheLinesSent === (TotalCacheLinesNeeded - 1.U) && 
                    io.FetchUnit.fire, true.B, false.B)
            }
        }
            */
}