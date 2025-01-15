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
    val Request = DecoupledIO(new TLBundleA(params)) // Send request to fetch unit 
}


case class RequestorFetchUnitPort(params: TLBundleParameters) extends Bundle
{
    val FetchReq = DecoupledIO(new TLBundleA(params))
}



class RequestorRME(params: RelMemParams, tlInEdge : TLEdge, tlOutEdge: TLEdge, tlOutBundle: TLBundle)(
    implicit p: Parameters) extends LazyModule {
        val tlOutParams = tlOutEdge.bundle
        val tlOutBeats = tlOutEdge.numBeats(tlOutBundle.a.bits)
        val tlInParams = tlInEdge.bundle
        val io = IO(new Bundle {
            // Fetch Unit Port
            val FetchUnit = RequestorFetchUnitPort(tlOutParams)

            // Control Unit Port
            val ControlUnit = Flipped(ControlUnitRequestorPort())

            // Config Port
            val Config = Input(RMEConfigPortIO())

            // Trapper Port
            val Trapper = Flipped(RequestorTrapperPort(tlInParams))

        })



         def divideCeil(a: UInt, b: UInt): UInt = {
            (a + b - 1.U) / b
        }


    lazy val module = new Impl
    class Impl extends LazyModuleImp(this) {
        val CacheLineSize = 64 // cache line size in bytes
        /*
            We operate on a single bit state machine:

            Idle => ready to receive incoming requests
            Active => generating new modified requests based on config to send to fetch unit
        */
        val active :: idle :: Nil = Enum(2)
        val stateReg = RegInit(idle)
        val requestQueue = Module(new Queue(new TLBundleA(tlOutParams), 16, flow=false))
        val baseRequest = Reg(new TLBundleA(tlOutParams))
        val ModifiedRequestsSent = RegInit(true.B) // track if we have sent all the necessary requests

        // What happens if enabled column count changes while we're handling request? 
        // I think this will lead to issues of incomplete request formation
        // We will load a new config here so it does not get modified mid request generation
        val CurrentRowSize = RegInit(0.U(32.W)) // size of each row in database
        val CurrentRowCount = RegInit(0.U(32.W))  // count of each row in database
        val CurrentEnabledColumnCount = RegInit(0.U(4.W))  // total number of enabled columns
        val CurrentColumnWidths = RegInit(Vec(15, 0.U(6.W))) // width of ith enabled column
        val CurrentColumnOffsets =  RegInit(Vec(15, 0.U(6.W))) // offset off column j from column j-1
        val CurrentFrameOffset = RegInit(0.U(32.W))

        // This should give us the total size in bytes we need to grab
        val TotalReqSize = CurrentColumnWidths.reduce( _ + _) + CurrentColumnOffsets.reduce(_ + _)
        val TotalCacheLinesNeeded = divideCeil(TotalReqSize, CacheLineSize.U) // if we have an offset > 64 we can skip a line


        // Default outputs
        stateReg := stateReg
        io.Trapper.Request.ready := ModifiedRequestsSent // ready for another request if we have sent all others

        

        // Set outputs for each state
        switch(stateReg)
        {
            is (idle) {
                io.Trapper.Request.ready := true.B
            }



            is (active) {
                

                
                
            }
        }



        // Next state logic
        switch(stateReg)
        {
            is (idle) {
                stateReg := Mux(io.Trapper.Request.fire, active, idle)





                ModifiedRequestsSent := Mux(io.Trapper.Request.fire, false.B, true.B)





                /*
                    Update config
                */
                CurrentRowSize              := io.Config.RowSize
                CurrentRowCount             := io.Config.RowCount
                CurrentEnabledColumnCount   := io.Config.EnabledColumnCount
                CurrentColumnWidths         := io.Config.ColumnWidths
                CurrentColumnOffsets        := io.Config.ColumnOffsets
                CurrentFrameOffset          := io.Config.FrameOffset


            }



            is (active) {
                stateReg := Mux(ModifiedRequestsSent, idle, active)








                /*
                    Update config before generating next request
                */
                CurrentRowSize              := Mux(ModifiedRequestsSent, io.Config.RowSize, CurrentRowSize)
                CurrentRowCount             := Mux(ModifiedRequestsSent, io.Config.RowCount, CurrentRowCount)
                CurrentEnabledColumnCount   := Mux(ModifiedRequestsSent, io.Config.EnabledColumnCount, CurrentEnabledColumnCount)
                CurrentColumnWidths         := Mux(ModifiedRequestsSent, io.Config.ColumnWidths, CurrentColumnWidths)
                CurrentColumnOffsets        := Mux(ModifiedRequestsSent, io.Config.ColumnOffsets, CurrentColumnOffsets)
                CurrentFrameOffset          := Mux(ModifiedRequestsSent, io.Config.FrameOffset, CurrentFrameOffset)



                //ModififiedRequestsSent := Mux(CurrentCol)
            }
        }

    }
}