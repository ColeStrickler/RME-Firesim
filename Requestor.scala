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




case class RequestorTrapperPort(params : TLBundleParameters) extends Bundle
{
    val Request = DecoupledIO(Output(new TLBundleA(params))) // Send request to fetch unit 
}


case class RequestorFetchUnitPort(params: TLBundleParameters) extends Bundle
{
    val FetchReq = Output(new TLBundleA(params))
    val isBaseRequest = Output(Bool())
}



class RequestorRME(params: RelMemParams, tlInEdge : TLEdge, tlOutEdge: TLEdge, tlOutBundle: TLBundle, instance: Int)(
    implicit p: Parameters) extends Module {
        val tlOutParams = tlOutEdge.bundle
        //val tlOutBeats = tlOutEdge.numBeats(tlOutBundle.a.bits)
        val tlInParams = tlInEdge.bundle

         def divideCeil(a: UInt, b: UInt): UInt = {
            (a + b - 1.U) / b
        }

        val io = IO(new Bundle {
            // Fetch Unit Port
            val FetchUnit = Decoupled(new RequestorFetchUnitPort(tlInParams))
            //val FetchReq = Decoupled(Output(new TLBundleA(tlInParams)))
            //val isBaseRequest = Output(Bool())

            // Control Unit Port
            //val ControlUnit = Flipped(ControlUnitRequestorPort())

            // Config Port
            val Config = Flipped(RMEConfigPortIO())

            // Trapper Port
            val Trapper = Flipped(RequestorTrapperPort(tlInParams))

        }).suggestName(s"requestorio_$instance")

        val CacheLineSize = 64 // cache line size in bytes
        /*
            We operate on a single bit state machine:

            Idle => ready to receive incoming requests
            Active => generating new modified requests based on config to send to fetch unit
        */
        val active :: idle :: Nil = Enum(2)
        val stateReg = RegInit(idle)
        val requestQueue = Module(new Queue(new TLBundleA(tlInParams), 16, flow=false))
        val baseRequest = Reg(new TLBundleA(tlOutParams))
        val ModifiedRequestsSent = WireInit(true.B) // track if we have sent all the necessary requests
        val readyNextReq = RegInit(true.B)

        // What happens if enabled column count changes while we're handling request? 
        // I think this will lead to issues of incomplete request formation
        // We will load a new config here so it does not get modified mid request generation
        val CurrentRowSize = RegInit(0.U(32.W)) // size of each row in database
        val CurrentRowCount = RegInit(0.U(32.W))  // count of each row in database
        val CurrentEnabledColumnCount = RegInit(0.U(4.W))  // total number of enabled columns
        val CurrentColumnWidths = RegInit(0.U(7.W)) // width of ith enabled column
        val CurrentColumnOffsets =  RegInit(VecInit(Seq.fill(15)(0.U(7.W)))) // offset off column j from column j-1
        val CurrentFrameOffset = RegInit(0.U(32.W))

        // This should give us the total size in bytes we need to grab
        val TotalCacheLinesNeeded = RegInit(0.U(8.W))
        val TotalCacheLinesSent = RegInit(0.U(4.W))
        


        when (io.FetchUnit.fire)
        {
            SynthesizePrintf("[REQUESTOR] ==> Sent request to fetch unit\n")
        }
        /* 
            Defaults
        */
        stateReg := stateReg
        baseRequest := baseRequest
        requestQueue.io.enq <> io.Trapper.Request // queue up requests to prevent stalls
        io.Trapper.Request.ready := requestQueue.io.enq.ready

        io.FetchUnit.valid := false.B // default to false
        io.FetchUnit.bits.FetchReq := baseRequest // default 
        io.FetchUnit.bits.isBaseRequest := false.B
        readyNextReq := ModifiedRequestsSent
        requestQueue.io.deq.ready := readyNextReq // start new requests when all of old ones have been sent
        

        

        // Set outputs for each state
        switch(stateReg)
        {
            is (idle) 
            {
            }
            is (active) 
            {
            }
        }



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
                io.FetchUnit.valid := sendRequest.valid
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

    
}