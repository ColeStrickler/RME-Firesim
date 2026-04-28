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




case class RequestTableEntry(inMaxID : Int, outMaxID : Int, nExtractDesc : Int) extends Bundle
{
    val descriptor = new RequestDescriptor(inMaxID, outMaxID)
    val extractionDescriptors = Vec(nExtractDesc, new ExtractionDescriptor(4))
    val activeDesc = UInt(log2Ceil(nExtractDesc+1).W)
}


case class FetchUnitControlPort(tlParams : TLBundleParameters, inMaxID : Int, outMaxID : Int, dataRegWidth: Int, nExtractDesc: Int = 16) extends Bundle
{
    val data = Output(UInt(512.W)) // 64 bytes = 1 cache line
    val reqTableEntry = Output(new RequestTableEntry(inMaxID, outMaxID, nExtractDesc))
}


case class FetchUnitIO(tlInParams: TLBundleParameters, tlOutParams: TLBundleParameters, inMaxID : Int, outMaxID : Int, dataRegWidth : Int) extends Bundle
{
// Requestor Port
        val Requestor = Flipped(Decoupled(new RequestorFetchUnitPort(tlInParams, tlOutParams, inMaxID, outMaxID))) // Receive address to request from the Requestor Module]
        //val FetchReq = Flipped(Decoupled(Output(new TLBundleA(tlInEdge.bundle))))
        //val isBaseRequest = Flipped(Output(Bool()))
        //val Requestor_isBaseRequest = Flipped(Decoupled(Bool()))
        //val Requestor_FetchReq = Flipped(Decoupled(new TLBundleA(tlInEdge.bundle)))


        /*
            We will probable want to tag this A channel request on as metadata so we can easily form
            D channel replies to cache
        */

        // DRAM Port
        val OutReq = Decoupled(new TLBundleA(tlOutParams)) // send outbound memory requests to DRAM
        val inReply = Flipped(Decoupled(new TLBundleD(tlOutParams))) // receive inbound data from DRAM
        // Control Unit Port
        val ControlUnit = Decoupled(FetchUnitControlPort(tlInParams, inMaxID, outMaxID, dataRegWidth))

}





class FetchUnitRME(params: RelMemParams, adapter: TLAdapterNode, cachedRegionEdge: TLEdgeIn, instance: Int, subInstance: Int)(
    implicit p: Parameters) extends Module {

        val (out, tlOutEdge) = adapter.out(0)
        val (in, tlInEdge) = adapter.in(0)
        val tlOutA = out.a
        val tlOutD = out.d
        val tlOutParams = tlOutEdge.bundle
        val tlInParams = cachedRegionEdge.bundle
        val inMaxID = (math.pow(2, cachedRegionEdge.bundle.sourceBits)-1).toInt
        val outMaxID = (math.pow(2, tlOutParams.sourceBits)-1).toInt
        val beatWidth = 8
        val dataRegWidth = (math.pow(2, params.maxDataSize+1)).toInt * beatWidth // this should give us the extra byte we need to extract excesses
        val srcID = (outMaxID - subInstance).U
        println(s"inMaxID $inMaxID outMaxID $outMaxID using SrCID ${outMaxID - subInstance}")
        println(s"dataRegWidth $dataRegWidth")
        val io = IO(new FetchUnitIO(tlInParams, tlOutParams, inMaxID, outMaxID, dataRegWidth)).suggestName(s"fetchunitio_$instance-$subInstance")


        io.OutReq.valid := false.B
        io.OutReq.bits := 0.U.asTypeOf(new TLBundleA(tlOutParams))

        // Maximum coalescable requests
        val nExtractDesc = 16
        val FREE_REQ_ENTRY = nExtractDesc+1


        // Request Table Init //
        val emptyEntry = Wire(new RequestTableEntry(inMaxID, outMaxID, nExtractDesc))
        emptyEntry.descriptor := 0.U.asTypeOf(RequestDescriptor(inMaxID, outMaxID))
        emptyEntry.extractionDescriptors := VecInit(Seq.fill(nExtractDesc) 
        { // extractionDescriptors init (each element must be initialized)
            val ed = Wire(new ExtractionDescriptor(4))
            ed.start := 0.U
            ed.size  := 0.U
            ed.pos   := 0.U
            ed
        })
        emptyEntry.activeDesc := FREE_REQ_ENTRY.U
                // Request Table Init //




        /*
            DATA REGISTERS
        */
        val dataReg = RegInit(0.U(512.W)) // store a single cache line we get from DRAM
        val receivingEntry = RegInit(0.U(log2Ceil(nExtractDesc).W))
        val dataRegFull = RegInit(false.B)
        val dataRegActive = RegInit(false.B)
        /*
            DATA REGISTERS
        */


        /*
            Store information on outbound requests here
        */
        val requestTable = RegInit(VecInit(Seq.fill(params.nFetchUnits)(emptyEntry)))
        // HELPER FUNCTIONS 


        def ResetEntry(entry: UInt) : Unit = {
            requestTable(entry) := emptyEntry
        }


        def GetCoalesceEntry(coalesceVec: Vec[Bool]) : UInt = {
            PriorityEncoder(coalesceVec)
        }

        def GetCoalesceVec(incomingDesc: RequestDescriptor) : Vec[Bool] = {
            VecInit(requestTable.zipWithIndex.map{ case (entry,i) =>
                when(entry.descriptor.addr === incomingDesc.addr && io.Requestor.fire) {
                    SynthesizePrintf("Addr Match descCond %d, idmath %d, retLock %d\n", (entry.activeDesc < nExtractDesc.U), (entry.descriptor.baseID === incomingDesc.baseID), (receivingEntry === i.U && !dataRegFull))
                }
                (entry.descriptor.addr === incomingDesc.addr) && (entry.activeDesc < nExtractDesc.U) && (entry.descriptor.baseID === incomingDesc.baseID)
            })
        }

        def CanCoalesce(coalesceVec: Vec[Bool]): Bool = {
            VecInit(coalesceVec.zipWithIndex.map { case (b, i) =>
                b && !(receivingEntry === i.U && dataRegFull)
            }).reduce(_ || _)
        }       

        val availableEntryVec = VecInit(requestTable.map{ entry =>
            entry.activeDesc === FREE_REQ_ENTRY.U
        })


        def HasAvailableEntries() : Bool = {
            availableEntryVec.reduce(_ || _)
        }

        def AvailableEntries() : Vec[Bool] = { // only use once to avoid duplicating comparator logic
            availableEntryVec
        }

        def FirstAvailableEntry() : UInt = {
            PriorityEncoder(AvailableEntries())
        }


        def AllocateEntry(entry: UInt, reqPort: RequestorFetchUnitPort) : Unit = {
            val reqTableEntry = Wire(new RequestTableEntry(inMaxID, outMaxID, nExtractDesc))
            reqTableEntry := 0.U.asTypeOf(new RequestTableEntry(inMaxID, outMaxID, nExtractDesc))
            reqTableEntry.descriptor := reqPort.descriptor
            reqTableEntry.extractionDescriptors(0) := reqPort.extractionDescriptor
            reqTableEntry.activeDesc := 1.U

            requestTable(entry) := reqTableEntry
        }


        def CoalesceEntry(entry: UInt, extractionDescript: ExtractionDescriptor) : Unit = {
            val active_desc = requestTable(entry).activeDesc
            requestTable(entry).activeDesc := active_desc + 1.U
            requestTable(entry).extractionDescriptors(active_desc) := extractionDescript
        }


        def DescriptorToOutReq(desc : RequestDescriptor, src: UInt) : TLBundleA = {
            val ret = Wire(new TLBundleA(tlOutParams))
            ret.opcode := TLMessages.Get
            ret.param := 0.U
            ret.size := 6.U
            ret.source := src
            ret.address := desc.addr
            ret.data := 0.U
            ret.mask := Fill(ret.mask.getWidth, 1.U(1.W)) // all valid
            ret.corrupt := false.B
            // safest defaults for structured fields
            ret.user := 0.U.asTypeOf(ret.user)
            ret.echo := 0.U.asTypeOf(ret.echo)
            ret
        }



        /*
            When we have an incoming request we have 3 options:
            1.  Zero flag is set, we can immediately forward the value itself to the ControlUnit.
                Still must allocate entry in RequestTable for easy arbitration.

            2. Cannot coalesce entry. So allocate new entry in request table.

            3. Can coalesce entry, so we add an extraction descriptor to the already existing entry
               We need to ensure that if this happens on same cycle as that data is being removed, it is handled properly
        */


        val hasOutReqToSend = RegInit(false.B)
        val outReqEntryToSend = RegInit(0.U(log2Ceil(nExtractDesc)))

        val entry = FirstAvailableEntry()
        val cvec = GetCoalesceVec(io.Requestor.bits.descriptor)
        val can_coalesce = CanCoalesce(cvec)
        val coalesce_entry = GetCoalesceEntry(cvec)

        when (io.Requestor.fire)
        {
            hasOutReqToSend := false.B
            io.OutReq.valid := false.B
            when (!can_coalesce)
            {
                SynthesizePrintf("No Coalesce 0x%x\n", io.Requestor.bits.descriptor.addr)
                AllocateEntry(entry, io.Requestor.bits)
                hasOutReqToSend := !io.OutReq.fire
                io.OutReq.valid := true.B
            }
            .otherwise {
                SynthesizePrintf("Coalesce Entry! 0x%x\n", io.Requestor.bits.descriptor.addr)
                CoalesceEntry(coalesce_entry, io.Requestor.bits.extractionDescriptor)
            }
            
            /* 
                We try to just pass the request throught,
                but if we can't we will try again the next cycle

                For now, when hasOutReqToSend is true, we will not take in another request
                ---> this will simplify things greatly
            */
            

            outReqEntryToSend := entry
            val src = outMaxID.U-entry
            io.OutReq.bits := DescriptorToOutReq(io.Requestor.bits.descriptor, src)
        }


        when (hasOutReqToSend)
        {
            val outReq = Wire(Decoupled(new TLBundleA(tlOutParams)))
            outReq.valid := true.B

            val src = outMaxID.U-outReqEntryToSend
            outReq.bits := DescriptorToOutReq(requestTable(outReqEntryToSend).descriptor, src)
            io.OutReq <> outReq
            val (a_first, a_last, a_done) = tlOutEdge.firstlast(outReq)
            hasOutReqToSend := !(a_last && io.OutReq.fire)
        }
        io.Requestor.ready := HasAvailableEntries() || can_coalesce

        
        when (io.OutReq.fire) {
            SynthesizePrintf("[FetchUnit]: OutReq.fire 0x%x\n", io.OutReq.bits.address)
        }

        val (d_first, d_last, d_done, _, d_count) = tlOutEdge.firstlast2(io.inReply)
        io.inReply.ready := false.B


        when (io.inReply.valid) {
            io.inReply.ready := !dataRegFull   // can not receive more replies until we have done something with current data
        }

        when (io.inReply.fire) {
            receivingEntry  := outMaxID.U - io.inReply.bits.source
        }


    


        // shift in new data
        val dataWidth = io.inReply.bits.data.getWidth
        val shiftNewData = io.inReply.bits.data //+ d_count // count is to test
        
        // we have to splice the data after shift because zeroes are put in the top
        dataReg := Mux(io.inReply.fire, Cat(shiftNewData, (dataReg >> dataWidth)((dataReg.getWidth - 1)-dataWidth, 0)), dataReg)
        dataRegFull := Mux(d_done, true.B, Mux(dataRegFull, !io.ControlUnit.fire, false.B))


       // receivingEntry := (outMaxID.U - io.inReply.bits.source)

        io.ControlUnit.valid := dataRegFull  // we can write valid data to SPM after receiving entire cache line
        io.ControlUnit.bits.reqTableEntry := requestTable(receivingEntry)
        io.ControlUnit.bits.data := dataReg


        


        when (io.ControlUnit.fire)
        {
            ResetEntry(receivingEntry)
            SynthesizePrintf("ToControlUnit: BaseReqSrc %d\n", requestTable(receivingEntry).descriptor.baseID)
        }

}