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





case class ControlUnitRequestorPort(tlParams : TLBundleParameters) extends Bundle
{
    val ID = Output(UInt(tlParams.sourceBits.W))
}

case class ControlUnitTrapperPort(tlParams : TLBundleParameters) extends Bundle 
{
    val baseReq = Output(new TLBundleA(tlParams))
    val cacheLine = Output(UInt(512.W)) 
}


// we only need the cached manager edge



/*
    Let's parameterize the width of this as well.

    We will have a single input into the control unit. We will output the active config #s along with the IDs

    We maybe can feed back the active config numbers to the Requestors such that they only output valid if that config is active 


    Let us actually reduce the size of the receiving fetch unit buffers. Lets place some asserts on them as well to reduce the size
*/

class ControlUnitRME(params: RelMemParams, tlOutEdge: TLEdgeOut, tlCachedEdge: TLEdgeIn, instance: Int) (
    implicit p: Parameters) extends Module {

    val tlParams = tlCachedEdge.bundle
    val tlOutParams = tlOutEdge.bundle
    val outMaxID = (math.pow(2, tlOutParams.sourceBits)-1).toInt
    val inMaxID = (math.pow(2, tlParams.sourceBits)-1).toInt
    val beatWidth = 8
    val dataRegWidth = (math.pow(2, params.maxDataSize+1)).toInt * beatWidth // this should give us the extra byte we need to extract excesses
    val io = IO(new Bundle{
        // Config Port 
        //val Config = Input(RMEConfigPortIO())


        // Fetch Unit Port
        val FetchUnitPort = Flipped(DecoupledIO(FetchUnitControlPort(tlParams, inMaxID, outMaxID, dataRegWidth)))
        val ID = Output(UInt(tlParams.sourceBits.W))
        val useID = Output(Bool())
        //val ticket = Output(UInt(16.W)) // help enforce ordering semantics

        // Trapper Port
        val TrapperPort = DecoupledIO(ControlUnitTrapperPort(tlParams))



        // Requestor Port
        //val RequestorPort = Valid(ControlUnitRequestorPort(tlParams))

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
        
        




            val currentlyPacking = RegInit(false.B)
            val BaseReq = Reg(new TLBundleA(tlParams))
            println("Ctrl unit basereq.source.width %d", io.FetchUnitPort.bits.baseReq.source.getWidth)
            val ColExtractor = Module(new ColumnExtractor(params, inMaxID, outMaxID))
            val packer = Module(new PackerRME(params, inMaxID, outMaxID))
            val descriptor = Reg(new RequestDescriptor(inMaxID, outMaxID))
            when (io.FetchUnitPort.fire)
            {
               // SynthesizePrintf("[ControlUnit] Fire in! io.FetchUnitPort.baseReq.address 0x%x, src %d\n", io.FetchUnitPort.bits.baseReq.address, io.FetchUnitPort.bits.baseReq.source)
            }


            io.ID := BaseReq.source
            io.useID := currentlyPacking



            when (currentlyPacking)
            {
            // SynthesizePrintf("[ControlUnit] packed %d/64 for addr: 0x%x --> ID=%d\n", packer.io.nPacked, BaseReq.address, BaseReq.source)
            // SynthesizePrintf("ColExtractor.io.CacheLineIn.ready %d, ctrl src %d\n",ColExtractor.io.CacheLineIn.ready, BaseReq.source)
            // SynthesizePrintf("Ctrl in ID %d\n", io.FetchUnitPort.bits.descriptor.baseID )
            }


            descriptor := Mux(io.FetchUnitPort.fire, io.FetchUnitPort.bits.descriptor, descriptor)

            ColExtractor.io.CacheLineIn.bits    := io.FetchUnitPort.bits.data
            ColExtractor.io.CacheLineIn.valid   := io.FetchUnitPort.fire
            ColExtractor.io.DescriptorIn        := io.FetchUnitPort.bits.descriptor


            // we modified this, and think this should work.if currently packing a line, we need to wait to pack the whole thing
            // we can add more packers eventually and arbitrate over the trapper port
            currentlyPacking := Mux(currentlyPacking, !io.TrapperPort.fire, io.FetchUnitPort.fire)
            /*
                Potential bottle neck?
            */



            io.FetchUnitPort.ready := ColExtractor.io.CacheLineIn.ready && (!currentlyPacking || io.FetchUnitPort.bits.baseReq.source === BaseReq.source)
            

            // this should fire after we get an entire cache line
            BaseReq := Mux(io.FetchUnitPort.fire, io.FetchUnitPort.bits.baseReq, BaseReq)  // --> need to make sure we can grab and use this correctly
            packer.io.ColExtractor <> ColExtractor.io.Packer

            io.TrapperPort.bits.baseReq := BaseReq
            //io.TrapperPort.bits.baseReq.source := descriptor.baseID
            
            io.TrapperPort.bits.cacheLine := packer.io.PackedLine.bits
            io.TrapperPort.valid := packer.io.PackedLine.valid
            packer.io.PackedLine.ready := io.TrapperPort.ready
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


            val config = io.FetchUnitPort.bits.descriptor.config
            //io.RequestorPort.zipWithIndex.foreach{ case (reqport, i) =>
//
            //    when (config === i.U)
            //    {
            //        reqport.bits.retireID := io.FetchUnitPort.bits.descriptor.allocID
            //        reqport.valid := io.FetchUnitPort.fire
            //    } .otherwise
            //    {
            //        reqport.bits := 0.U.asTypeOf(ControlUnitRequestorPort(outMaxID))    
            //        reqport.valid := false.B
            //    }
            //}






}