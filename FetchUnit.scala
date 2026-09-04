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
    val extractionDescriptorsValid = Vec(nExtractDesc, Bool())
    //val activeDesc = UInt(log2Ceil(nExtractDesc+1).W)
    val active = Bool()
}


case class FetchUnitControlPort(tlParams : TLBundleParameters, inMaxID : Int, outMaxID : Int, dataRegWidth: Int, nExtractDesc: Int = 16) extends Bundle
{
    val data = Output(UInt(512.W)) // 64 bytes = 1 cache line
    val reqTableEntry = Output(new RequestTableEntry(inMaxID, outMaxID, nExtractDesc))
}


case class FetchUnitIO(tlInParams: TLBundleParameters, tlOutParams: TLBundleParameters, inMaxID : Int, outMaxID : Int, dataRegWidth : Int, toCacheParams: TLBundleParameters) extends Bundle
{
// Requestor Port
        val Requestor = Flipped(Decoupled(new RequestorFetchUnitPort(inMaxID, outMaxID))) // Receive address to request from the Requestor Module]

        val Prefetch = Flipped(new FetchUnitPrefetchUnitPort(inMaxID, outMaxID))
        //val FetchReq = Flipped(Decoupled(Output(new TLBundleA(tlInEdge.bundle))))
        //val isBaseRequest = Flipped(Output(Bool()))
        //val Requestor_isBaseRequest = Flipped(Decoupled(Bool()))
        //val Requestor_FetchReq = Flipped(Decoupled(new TLBundleA(tlInEdge.bundle)))


        // LLC Port
        val IncomingReqInCache = Input(Bool())
        val LLCOutReq = Decoupled(new TLBundleA(toCacheParams)) // send outbound memory requests to DRAM
        val LLCInReply = Flipped(Decoupled(new TLBundleD(toCacheParams))) // receive inbound data from DRAM


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





class FetchUnitRME(params: RelMemParams, adapter: TLAdapterNode, cachedRegionEdge: TLEdgeIn, toCacheNode : TLClientNode, instance: Int, subInstance: Int)(
    implicit p: Parameters) extends Module {

        val (out, tlOutEdge) = adapter.out(0)
        val (in, tlInEdge) = adapter.in(0)

        val (toCacheOutBundle, toCacheOutTLInEdge) = toCacheNode.out(0)
        val toCacheTLParams = toCacheOutTLInEdge.bundle

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
        val io = IO(new FetchUnitIO(tlInParams, tlOutParams, inMaxID, outMaxID, dataRegWidth, toCacheTLParams)).suggestName(s"fetchunitio_$instance-$subInstance")

        println(s"\n\n\nout data width ${io.OutReq.bits.data.getWidth}\n\n\n")
        io.OutReq.valid := false.B
        io.OutReq.bits := 0.U.asTypeOf(new TLBundleA(tlOutParams))

        io.LLCOutReq.valid := false.B
        io.LLCOutReq.bits := 0.U.asTypeOf(new TLBundleA(toCacheTLParams))

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
            //ed.size  := 0.U
            ed.pos   := 0.U
            ed
        })
        emptyEntry.active := false.B
        emptyEntry.extractionDescriptorsValid := VecInit(Seq.fill(nExtractDesc)(false.B))
        //emptyEntry.activeDesc := 0.U
                // Request Table Init //




        /*
            DATA REGISTERS
        */
        val dataReg = RegInit(0.U(512.W)) // store a single cache line we get from DRAM
        val dataReg2 = RegInit(0.U(512.W)) // store a single cache line we get from DRAM
        val receivingEntry = RegInit(0.U(log2Ceil(params.nFetchUnits).W))
        val receivingEntry2 = RegInit(0.U(log2Ceil(params.nFetchUnits).W))
        val dataRegFull = RegInit(false.B)
        val dataRegActive = RegInit(false.B)
        val dataReg2Full = RegInit(false.B)
        val dataReg2Active = RegInit(false.B)
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
                (entry.descriptor.addr === incomingDesc.addr) && (incomingDesc.dst.asUInt === 0.U) && (entry.descriptor.baseID === incomingDesc.baseID)
            })
        }

        def CanCoalesce(coalesceVec: Vec[Bool]): Bool = {
            VecInit(coalesceVec.zipWithIndex.map { case (b, i) =>
                b && !(receivingEntry === i.U && dataRegFull) && !(receivingEntry2 === i.U && dataReg2Full)
            }).reduce(_ || _)
        }       

        val availableEntryVec = VecInit(requestTable.map{ en =>
            !en.active
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
            val pos = reqPort.extractionDescriptor.pos
            reqTableEntry.extractionDescriptors(pos) := reqPort.extractionDescriptor
            reqTableEntry.extractionDescriptorsValid(pos) := true.B
            reqTableEntry.active := true.B
            requestTable(entry) := reqTableEntry
        }


        def CoalesceEntry(entry: UInt, extractionDescript: ExtractionDescriptor) : Unit = {
            //val active_desc = requestTable(entry).activeDesc
            val pos = extractionDescript.pos
            //requestTable(entry).activeDesc := active_desc + 1.U
            //SynthesizePrintf("(FetchUnit) pos incoming %d\n", pos)
            requestTable(entry).extractionDescriptors(pos) := extractionDescript
            assert(!requestTable(entry).extractionDescriptorsValid(pos))
            requestTable(entry).extractionDescriptorsValid(pos) := true.B
        }


        def DescriptorToOutReq(desc: RequestDescriptor, src: UInt): TLBundleA = {
            val (legal, ret) = tlOutEdge.Get(          // use the edge you already have!
                fromSource = src,
                toAddress  = desc.addr,
                lgSize     = 6.U
            )
            // legal should be true — add assert(legal) in synthesis if you want

            ret  // the helper already sets opcode, param, size, address, mask, data=0, corrupt=false, etc. correctly
        }


        def DescriptorToCacheReq(desc: RequestDescriptor, src: UInt): TLBundleA = {
            val (legal, ret) = toCacheOutTLInEdge.Get(          // use the edge you already have!
                fromSource = src, // may need to do something different with the sources here....
                toAddress  = desc.addr,
                lgSize     = 6.U
            )
            // legal should be true — add assert(legal) in synthesis if you want
            assert(legal)
            ret  // the helper already sets opcode, param, size, address, mask, data=0, corrupt=false, etc. correctly
        }



        println(s"(FetchUnit) MASK ${new TLBundleA(tlOutParams).mask.getWidth}")
        println(s"(FetchUnit) DATA ${new TLBundleA(tlOutParams).data.getWidth}")

        /*
            When we have an incoming request we have 3 options:
            1.  Zero flag is set, we can immediately forward the value itself to the ControlUnit.
                Still must allocate entry in RequestTable for easy arbitration.

            2. Cannot coalesce entry. So allocate new entry in request table.

            3. Can coalesce entry, so we add an extraction descriptor to the already existing entry
               We need to ensure that if this happens on same cycle as that data is being removed, it is handled properly
        */


        val hasOutReqToSend = RegInit(false.B)
        val outReqEntryToSend = RegInit(0.U(log2Ceil(params.nFetchUnits).W))
        val hasOutReqToCache = RegInit(false.B)
        val alloc_entry = FirstAvailableEntry()
        val cvec = GetCoalesceVec(io.Requestor.bits.descriptor)
        val can_coalesce = CanCoalesce(cvec)
        val coalesce_entry = GetCoalesceEntry(cvec)
        hasOutReqToSend := false.B
        outReqEntryToSend := outReqEntryToSend
        io.OutReq.valid := false.B
        hasOutReqToCache := false.B



        assert((math.pow(2, toCacheTLParams.sourceBits)) >= params.nFetchUnits) // otherwise we would use source IDs larger than we can accomodate
        when (io.Requestor.fire)
        {
            SynthesizePrintf("incoming 0x%x -- dst %d coalesce %d  baseID %d\n", io.Requestor.bits.descriptor.addr, io.Requestor.bits.descriptor.dst.asUInt, can_coalesce, io.Requestor.bits.descriptor.baseID)
            when (!can_coalesce)
            {
                //SynthesizePrintf("No Coalesce 0x%x, Allocate entry %d DescState %d BaseID %d\n", io.Requestor.bits.descriptor.addr, alloc_entry, requestTable(alloc_entry).active, io.Requestor.bits.descriptor.baseID)
                AllocateEntry(alloc_entry, io.Requestor.bits)
                hasOutReqToSend := !io.LLCOutReq.fire && !io.OutReq.fire
                hasOutReqToCache := io.IncomingReqInCache && !io.LLCOutReq.fire
                io.OutReq.valid := !io.IncomingReqInCache
                outReqEntryToSend := alloc_entry
                val src = outMaxID.U-alloc_entry
                io.OutReq.bits := DescriptorToOutReq(io.Requestor.bits.descriptor, src)

                io.LLCOutReq.bits := DescriptorToCacheReq(io.Requestor.bits.descriptor, alloc_entry)
                io.LLCOutReq.valid := io.IncomingReqInCache

                when (io.OutReq.fire) {
                 //   SynthesizePrintf("[FetchUnit] io.OutReq.fire baseReq %d\n", io.Requestor.bits.descriptor.baseID)
                }

            }
            .otherwise {
                //SynthesizePrintf("(FetchUnit) incoming source %d\n", io.Requestor.bits.descriptor.dst.asUInt)
                //SynthesizePrintf("Coalesce Entry! 0x%x BaseID %d Entry %d\n", io.Requestor.bits.descriptor.addr, io.Requestor.bits.descriptor.baseID, coalesce_entry)
                CoalesceEntry(coalesce_entry, io.Requestor.bits.extractionDescriptor)
            }
            
            /* 
                We try to just pass the request throught,
                but if we can't we will try again the next cycle

                For now, when hasOutReqToSend is true, we will not take in another request
                ---> this will simplify things greatly
            */
        }


        when (hasOutReqToSend)
        {
          //  SynthesizePrintf("HasOutReqEntryToSend %d\n", outReqEntryToSend)
            val outReq = Wire(new TLBundleA(tlOutParams))
            val outReqToCache = Wire(new TLBundleA(toCacheTLParams))

            val src = outMaxID.U-outReqEntryToSend
            outReq := DescriptorToOutReq(requestTable(outReqEntryToSend).descriptor, src)
            outReqToCache := DescriptorToCacheReq(requestTable(outReqEntryToSend).descriptor, outReqEntryToSend)
            io.OutReq.valid := hasOutReqToSend && !hasOutReqToCache
            io.OutReq.bits := outReq

            io.LLCOutReq.valid := hasOutReqToSend && hasOutReqToCache
            io.LLCOutReq.bits := outReqToCache


            when (hasOutReqToSend && io.OutReq.fire) {
            //    SynthesizePrintf("[FetchUnit]: outReqEntry %d hasOutReqToSend.fire baseReq %d\n", outReqEntryToSend, requestTable(outReqEntryToSend).descriptor.baseID)
            }

            val (a_first, a_last, a_done) = tlOutEdge.firstlast(io.OutReq)
            hasOutReqToSend := !io.OutReq.fire && !io.LLCOutReq.fire
        }
        /*
            Basically we need to figure out if we can reuse the same src scheme for requests into the cache.

            Once we know this, we can figure out if we need to handle them together or separately.
        
        
        */    

    
        io.Requestor.ready := (HasAvailableEntries() && !hasOutReqToSend) || can_coalesce 

        
        when (io.OutReq.fire) {
        //SynthesizePrintf("[FetchUnit]: OutReq.fire 0x%x src %d\n", io.OutReq.bits.address, io.OutReq.bits.source)
             // SynthesizePrintf(
 //   "[DTU-A] addr=0x%x size=%d mask=0x%x beatFirst=%d beatLast=%d\n",
 //   io.OutReq.bits.address,
 //   io.OutReq.bits.size,
 //   io.OutReq.bits.mask,
 //   tlOutEdge.firstlast(io.OutReq)._1,
 //   tlOutEdge.firstlast(io.OutReq)._2
 // )
        }

        val (d_first, d_last, d_done, _, d_count) = tlOutEdge.firstlast2(io.inReply)
        val (d_first_cache, d_last_cache, d_done_cache, _, d_count_cache) = tlOutEdge.firstlast2(io.LLCInReply)

        io.inReply.ready := false.B
       // io.LLCInReply.ready := false.B

        // we give preference to the cache
        val DRAMInReplyActive  = RegInit(false.B)
        val LLCInReplyActive = RegInit(false.B)

        io.inReply.ready := !dataRegFull  // can not receive more replies until we have done something with current data
        io.LLCInReply.ready := !dataReg2Full 

        when (io.inReply.fire) {
            SynthesizePrintf("[FetchUnit]: inReply.firesrc %d (%d,%d,%d) dataRegfull %d\n", io.inReply.bits.source, d_first_cache, d_last_cache, d_done_cache, dataReg2Full)
        }
        when (d_first && io.inReply.fire) {
            receivingEntry  := outMaxID.U - io.inReply.bits.source
        }

        when (d_first_cache && io.LLCInReply.fire) {
            receivingEntry2  := io.LLCInReply.bits.source
        }


        // shift in new data
        val dataWidth = io.inReply.bits.data.getWidth
        val shiftNewData = io.inReply.bits.data //+ d_count // count is to test

        val dataWidthCache = io.LLCInReply.bits.data.getWidth
        val shiftNewDataCache = io.LLCInReply.bits.data
        val outputFire = io.Prefetch.ToPre.fire || io.ControlUnit.fire


        dataReg2 := Mux(io.LLCInReply.fire, Cat(shiftNewDataCache, (dataReg2 >> dataWidthCache)((dataReg2.getWidth - 1)-dataWidthCache, 0)), dataReg2)
        dataReg2Full := Mux(
            dataReg2Full,
            !(outputFire && !dataRegFull),
            d_last_cache && io.LLCInReply.fire
        )
        
        // we have to splice the data after shift because zeroes are put in the top
        dataReg := Mux(io.inReply.fire, Cat(shiftNewData, (dataReg >> dataWidth)((dataReg.getWidth - 1)-dataWidth, 0)), dataReg)
        //dataRegFull := Mux(dataRegFull, !(io.Prefetch.ToPre.fire || io.ControlUnit.fire), (d_last && io.inReply.fire))
        dataRegFull := Mux(
            dataRegFull,
            !outputFire,
            d_last && io.inReply.fire
        )

       // receivingEntry := (outMaxID.U - io.inReply.bits.source)


        val currTableEntry = Mux(dataRegFull, requestTable(receivingEntry), requestTable(receivingEntry2))
        val hasFullData = dataRegFull || dataReg2Full
        val fullDataToSend = Mux(dataRegFull, dataReg, dataReg2)

        io.ControlUnit.valid := Mux(currTableEntry.descriptor.dst === DESTINATION.CONTROL_UNIT, hasFullData, false.B)  // we can write valid data to SPM after receiving entire cache line
        io.ControlUnit.bits.reqTableEntry := currTableEntry
        io.ControlUnit.bits.data := fullDataToSend


        io.Prefetch.ToPre.valid := Mux(currTableEntry.descriptor.dst === DESTINATION.PREFETCH_UNIT, hasFullData, false.B)
        io.Prefetch.ToPre.bits.config := currTableEntry.descriptor.requestPlacement // we store this here since it is otherwise unused
        io.Prefetch.ToPre.bits.addr := currTableEntry.descriptor.addr
        io.Prefetch.ToPre.bits.data := fullDataToSend

        when (io.LLCOutReq.fire)
        {
            SynthesizePrintf("(FetchUnit) TO LLC\n")
        }

        when (io.LLCInReply.fire)
        {
            SynthesizePrintf("(FetchUnit) FROM LLC 0x%x\n", io.LLCInReply.bits.data)
        }


        when (io.OutReq.fire)
        {
            SynthesizePrintf("(FetchUnit) TO DRAM\n")
        }

        when (io.inReply.fire)
        {
            SynthesizePrintf("(FetchUnit) FROM DRAM 0x%x\n", io.LLCInReply.bits.data)
        }



        when (io.LLCInReply.valid)
        {
            SynthesizePrintf("(FetchUnit) FROM LLC VALID\n")
        }


        when (io.ControlUnit.fire || io.Prefetch.ToPre.fire)
        {
            when (dataRegFull)
            {
                ResetEntry(receivingEntry)
            }.elsewhen(dataReg2Full)
            {
                ResetEntry(receivingEntry2)
            }

            
            when (io.ControlUnit.fire)
            {
           //     SynthesizePrintf("ToControlUnit: receiving entry %d, BaseReqSrc %d\n", receivingEntry, requestTable(receivingEntry).descriptor.dst.asUInt)
            }
            .otherwise
            {
               // SynthesizePrintf("ToPrefetchUnit: receiving entry %d, BaseReqSrc %d data 0x%x\n", receivingEntry, requestTable(receivingEntry).descriptor.dst.asUInt, dataReg)
            }
        }
}



