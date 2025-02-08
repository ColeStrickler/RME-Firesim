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
import org.apache.commons.compress.java.util.jar.Pack200.Packer
import com.fasterxml.jackson.databind.JsonSerializable.Base







case class ControlUnitRequestorPort(maxID : Int) extends Bundle
{
    val retireID = Output(UInt(log2Ceil(maxID).W))
}

case class ControlUnitTrapperPort(tlParams : TLBundleParameters) extends Bundle 
{
    val baseReq = Output(new TLBundleA(tlParams))
    val cacheLine = Output(UInt(512.W)) 
}




class ControlUnitRME(params: RelMemParams, tlOutEdge: TLEdge, tlOutBundle: TLBundle, instance: Int)(
    implicit p: Parameters) extends Module {

    val tlParams = tlOutEdge.bundle
    val maxID = (math.pow(2, tlParams.sourceBits)-1).toInt
    val io = IO(new Bundle{
        // Config Port 
        //val Config = Input(RMEConfigPortIO())


        // Fetch Unit Port
        val FetchUnitPort = Flipped(DecoupledIO(FetchUnitControlPort(tlOutEdge.bundle, maxID)))


        // Trapper Port
        val TrapperPort = DecoupledIO(ControlUnitTrapperPort(tlParams))



        // Requestor Port
        val RequestorPort = Decoupled(ControlUnitRequestorPort(maxID))

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
        
        
        when (io.FetchUnitPort.fire)
        {
            SynthesizePrintf("[CONTROL UNIT FETCH UNIT FIRE] io.FetchUnitPort.baseReq.address 0x%x, data: 0x%x\n", io.FetchUnitPort.bits.baseReq.address, io.FetchUnitPort.bits.data)
        }
        val currentlyPacking = RegInit(false.B)
        val BaseReq = Reg(new TLBundleA(tlParams))
        val ColExtractor = Module(new ColumnExtractor(maxID))
        val packer = Module(new PackerRME(maxID))


        ColExtractor.io.CacheLineIn.bits := io.FetchUnitPort.bits.data
        ColExtractor.io.CacheLineIn.valid := io.FetchUnitPort.fire
        ColExtractor.io.DescriptorIn := io.FetchUnitPort.bits.descriptor


        // we modified this, and think this should work.if currently packing a line, we need to wait to pack the whole thing
        // we can add more packers eventually and arbitrate over the trapper port
        currentlyPacking := Mux(currentlyPacking, !io.TrapperPort.fire, io.FetchUnitPort.fire)
        io.FetchUnitPort.ready := ColExtractor.io.CacheLineIn.ready && (!currentlyPacking || io.FetchUnitPort.bits.descriptor.baseID === BaseReq.source)


        // this should fire after we get an entire cache line
        BaseReq := Mux(io.FetchUnitPort.fire, io.FetchUnitPort.bits.baseReq, BaseReq)  // --> need to make sure we can grab and use this correctly
        packer.io.ColExtractor <> ColExtractor.io.Packer
        io.TrapperPort.bits.baseReq := BaseReq
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
        io.RequestorPort.bits.retireID := io.FetchUnitPort.bits.descriptor.allocID
        io.RequestorPort.valid := io.FetchUnitPort.fire
        val ready = WireInit(false.B)
        ready := io.RequestorPort.ready 

}