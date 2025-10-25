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





case class ControlUnitRequestorPort(maxID : Int) extends Bundle
{
    val retireID = Output(UInt(log2Ceil(maxID).W))
}

case class ControlUnitTrapperPort(tlParams : TLBundleParameters) extends Bundle 
{
    val baseReq = Output(new TLBundleA(tlParams))
    val cacheLine = Output(UInt(512.W)) 
}


// we only need the cached manager edge

class ControlUnitRME(params: RelMemParams, tlOutEdge: TLEdgeOut, tlCachedEdge: TLEdgeIn, instance: Int) (
    implicit p: Parameters) extends Module {

    val tlParams = tlCachedEdge.bundle
    val tlOutParams = tlOutEdge.bundle
    val outMaxID = (math.pow(2, tlOutParams.sourceBits)-1).toInt
    val inMaxID = (math.pow(2, tlParams.sourceBits)-1).toInt
    val io = IO(new Bundle{
        // Config Port 
        //val Config = Input(RMEConfigPortIO())


        // Fetch Unit Port
        val FetchUnitPort = Vec(params.maxConfigs, Flipped(DecoupledIO(FetchUnitControlPort(tlParams, inMaxID, outMaxID))))
        val ID = Vec(params.maxConfigs, Output(UInt(tlParams.sourceBits.W)))
        val useID = Vec(params.maxConfigs, Output(Bool()))

        // Trapper Port
        val TrapperPort = Vec(params.maxConfigs, DecoupledIO(ControlUnitTrapperPort(tlParams)))



        // Requestor Port
        val RequestorPort = Vec(params.maxConfigs, Decoupled(ControlUnitRequestorPort(outMaxID)))

    }).suggestName(s"ctrlrio_$instance")
    
    /*
        We need to orchestrate the following:

        1. When a packed line can be sent back to trapper
        2. When a packed line is being written back, to write it to SPM and then back to memory
        3. When a new request comes in, we need to update its metadata entry in the Metadata SPM
        4. When a reply comes from the fetch unit, we need to write it to the Data SPM
        5. When we have all data needed to construct a packed line, we notify the packer
        6. Track where to write and read data from
    
    */


        //val spm = Module(new ScratchPadRME(params))
        
        //SynthesizePrintf("[CONTROL UNIT] ==> io.FetchUnitPort.ready %d, io.FetchUnitPort.valid %d\n", io.FetchUnitPort.ready, io.FetchUnitPort.valid)
        //SynthesizePrintf("[CONTROL UNIT] ==> io.TrapperPort.ready %d, io.TrapperPort.valid %d\n", io.TrapperPort.ready, io.TrapperPort.valid)
        
        




        for (i <- 0 until params.maxConfigs)
        {

            val currentlyPacking = RegInit(false.B)
            val BaseReq = Reg(new TLBundleA(tlParams))
            val ColExtractor = Module(new ColumnExtractor(inMaxID, outMaxID))
            val packer = Module(new PackerRME(inMaxID, outMaxID))
            val descriptor = Reg(new RequestDescriptor(inMaxID, outMaxID))
            when (io.FetchUnitPort(i).fire)
            {
                SynthesizePrintf("[ControlUnit_%d] io.FetchUnitPort.baseReq.address 0x%x\n", i.U, io.FetchUnitPort(i).bits.baseReq.address)
            }


            io.ID(i) := BaseReq.source
            io.useID(i) := currentlyPacking

            when (currentlyPacking)
            {
             SynthesizePrintf("[ControlUnit_%d] packed %d/64 for addr: 0x%x\n", i.U, packer.io.nPacked, BaseReq.address)
            // SynthesizePrintf("ColExtractor.io.CacheLineIn.ready %d, ctrl src %d\n",ColExtractor.io.CacheLineIn.ready, BaseReq.source)
            // SynthesizePrintf("Ctrl in ID %d\n", io.FetchUnitPort.bits.descriptor.baseID )
            }


            descriptor := Mux(io.FetchUnitPort(i).fire, io.FetchUnitPort(i).bits.descriptor, descriptor)

            ColExtractor.io.CacheLineIn.bits    := io.FetchUnitPort(i).bits.data
            ColExtractor.io.CacheLineIn.valid   := io.FetchUnitPort(i).fire
            ColExtractor.io.DescriptorIn        := io.FetchUnitPort(i).bits.descriptor


            // we modified this, and think this should work.if currently packing a line, we need to wait to pack the whole thing
            // we can add more packers eventually and arbitrate over the trapper port
            currentlyPacking := Mux(currentlyPacking, !io.TrapperPort(i).fire, io.FetchUnitPort(i).fire)
            /*
                Potential bottle neck?
            */



            io.FetchUnitPort(i).ready := ColExtractor.io.CacheLineIn.ready && (!currentlyPacking || io.FetchUnitPort(i).bits.descriptor.baseID === BaseReq.source)
            

            // this should fire after we get an entire cache line
            BaseReq := Mux(io.FetchUnitPort(i).fire, io.FetchUnitPort(i).bits.baseReq, BaseReq)  // --> need to make sure we can grab and use this correctly
            packer.io.ColExtractor <> ColExtractor.io.Packer

            io.TrapperPort(i).bits.baseReq := BaseReq
            //io.TrapperPort.bits.baseReq.source := descriptor.baseID
            
            io.TrapperPort(i).bits.cacheLine := packer.io.PackedLine.bits
            io.TrapperPort(i).valid := packer.io.PackedLine.valid
            packer.io.PackedLine.ready := io.TrapperPort(i).ready
            /*
                Column extractor takes data out of the incoming lines and sends it to packer
            */



            /*
                When packer has fully backed the line, we store to SPM and send it to trapper
                so it can be sent back to memory


                By storing packed data, we have all or nothing access, save space, and can more easily check whether or not
                the line has been assembled
            */


            /*
                We can now retire the ID that was allocated for this request
            */
            io.RequestorPort(i).bits.retireID := io.FetchUnitPort(i).bits.descriptor.allocID
            io.RequestorPort(i).valid := io.FetchUnitPort(i).fire
            val ready = WireInit(false.B)
            ready := io.RequestorPort(i).ready 
        }




}