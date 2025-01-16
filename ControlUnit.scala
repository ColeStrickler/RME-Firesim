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







case class ControlUnitRequestorPort() extends Bundle
{

}

case class ControlUnitTrapperPort() extends Bundle 
{

}




class ControlUnitRME(params: RelMemParams, tlOutEdge: TLEdge, tlOutBundle: TLBundle)(
    implicit p: Parameters) extends LazyModule {
    val io = IO(new Bundle{
        // Config Port 
        val Config = Input(RMEConfigPortIO())


        // Fetch Unit Port
        val FetchUnitPort = DecoupledIO(Flipped(FetchUnitControlPort()))


        // Trapper Port
        val TrapperPort = ControlUnitTrapperPort()



        // Requestor Port
        val RequestorPort = ControlUnitRequestorPort()

    })

    /*
        We need to orchestrate the following:

        1. When a packed line can be sent back to trapper
        2. When a packed line is being written back, to write it to SPM and then back to memory
        3. When a new request comes in, we need to update its metadata entry in the Metadata SPM
        4. When a reply comes from the fetch unit, we need to write it to the Data SPM
        5. When we have all data needed to construct a packed line, we notify the packer
        6. Track where to write and read data from
    
    */


    lazy val module = new Impl
    class Impl extends LazyModuleImp(this) {

        val spm = Module(new ScratchPadRME(params))



        /*
            Column extractor takes data out of the incoming lines and sends it to packer
        */



        /*
            When packer has fully backed the line, we store to SPM and send it to trapper
            so it can be sent back to memory


            By storing packed data, we have all or nothing access, save space, and can more easily check whether or not
            the line has been assembled
        */


        val data = io.FetchUnitPort.bits.data // should actually come from packer
        val dataAddr = io.FetchUnitPort.bits.baseAddr // this is base address of request
        val dataOffset = dataAddr - params.rmeaddress.U  // we use this to get store location
        val dataStoreAddr = dataOffset % params.DataSPMSize.U // this is basic hash function that gets us our storage location

       // spm.io.dataSPMIO.

        //io.FetchUnitPort.bits.data

    }

}