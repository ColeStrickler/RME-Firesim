package subsystem.rme
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLBundleA
import freechips.rocketchip.regmapper._
import freechips.rocketchip
import midas.targetutils.SynthesizePrintf
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import freechips.rocketchip.diplomacy.BufferParams.flow
import freechips.rocketchip.tilelink.TLMessages.AccessAck
import freechips.rocketchip.tilelink.TLMessages.AccessAckData
import freechips.rocketchip.diplomacy.{AddressRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.subsystem.{BaseSubsystem, MBUS, Attachable}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.subsystem.Attachable
import _root_.subsystem.rme.subsystem.rme.ConditionalDemuxD
import _root_.subsystem.rme.subsystem.rme.ConditionalDemuxA
import chisel3.util.RRArbiter
import _root_.subsystem.rme.FetchUnitRME

case class RelMemParams (
    regaddress: Int = 0x3000000,
    rmeaddress: BigInt = 0x110000000L,
    rmeAddressSize: BigInt = 0xfffffff,
    controlBeatBytes : Int = 8,
    DataSPMSize : Int = 1024,
    MetadataSPMSize : Int = 1024,
    nFetchUnits : Int = 4,
    inBoundXbar : Option[TLXbar] = None,
    //minSource : Int = 16
)



case object RMEKey extends Field[Option[RelMemParams]](None)


class RME(params: RelMemParams)(implicit p: Parameters) extends LazyModule
{

    val addr = Seq(AddressSet(params.rmeaddress, params.rmeAddressSize))
    
    val device = new SimpleDevice("relmem",Seq("ku-csl,relmem")) with HasReservedAddressRange {
    
    }

    /*
      We need this to reserve an address range in the device tree 
      -> This required modifications to the device tree generation. See RocketChip fork
    */
    ResourceBinding {
      Resource(device, "reserved").bind(ResourceAddress(addr, rocketchip.resources.ResourcePermissions(true, true, false, true, true)))
    }
    
  val node = TLAdapterNode()



    def ToRME(addr : UInt) : Bool = {
        val torme : Bool = addr >= params.rmeaddress.U &&  addr <= (params.rmeaddress.U + ((params.rmeAddressSize + 1)/2).U)
        torme
    }

    def UnmaskedAddress(addr: UInt) : UInt = {
        val unmaskedAddr = addr + ((params.rmeAddressSize + 1)/2).U
        unmaskedAddr
    }

    val ctlnode = TLRegisterNode(
        address     = Seq(AddressSet(params.regaddress, 0xfff)),
        device      = device,
        concurrency = 1, // Only one flush at a time (else need to track who answers)
        beatBytes   = params.controlBeatBytes)
     
  println("\n\n\n\nUsing relational memory engine\n\n\n\n")
  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val nClients = node.in.length
    require(nClients >= 1)
    println(s"Number of edges into RME: $nClients\n")
    

    for (i <- 0 until nClients)
    {
      val config = Wire(RMEConfigPortIO())
     // Registers
        val r_RowSize = RegInit(0.U(32.W))
        val r_RowCount = RegInit(0.U(32.W))
        val r_EnabledColumnCount = RegInit(0.U(4.W))
        val r_ColumnWidths = RegInit(0.U(6.W))
        val r_ColumnOffsets = RegInit(VecInit(Seq.fill(15)(0.U(7.W))))
        val r_FrameOffset = RegInit(0.U(32.W))
        val r_Reset = RegInit(false.B)
        val r_EnableRME = RegInit(false.B)

    /*
            Register next state assignment
        */
        when (r_Reset) // Synchronous High Reset
        {
            // r_Reset := false.B --> we will make software toggle the reset
            r_EnableRME := false.B
            r_RowSize := 0.U
            r_RowCount := 0.U
            r_EnabledColumnCount := 0.U
            r_FrameOffset := 0.U
            r_ColumnWidths := 0.U
            for (i <- 0 until 15)
            { 
                r_ColumnOffsets(i) := 0.U
            }
        }



        // Assign IO

        config.RowSize := r_RowSize
        config.RowCount := r_RowCount
        config.EnabledColumnCount := r_EnabledColumnCount
        config.FrameOffset := r_FrameOffset
        config.ColumnWidths := r_ColumnWidths
        config.Enabled := r_EnableRME

        for (i <- 0 until r_ColumnOffsets.length)
        {
            config.ColumnOffsets(i) := r_ColumnOffsets(i)
        }

        //when (r_EnableRME)
        //{
        //    SynthesizePrintf("RME Enabled\n")
        //}
        


      println("MAPPING RME CONTROL REGISTERS")
      // MMIO Register Mapping
      val mmio_Enable = Seq((0x00) -> Seq(RegField(r_EnableRME.getWidth, r_EnableRME, RegFieldDesc("enableRME", "enableRME"))))
      val mmio_RowSize = Seq((0x10) -> Seq(RegField(r_RowSize.getWidth, r_RowSize, RegFieldDesc("RowSize", "RowSizeRME"))))
      val mmio_RowCount = Seq((0x20) -> Seq(RegField(r_RowCount.getWidth, r_RowCount, RegFieldDesc("RowCount", "RowCountRME"))))
      val mmio_EnabledColumnCount = Seq((0x30) -> Seq(RegField(r_EnabledColumnCount.getWidth, r_EnabledColumnCount, RegFieldDesc("EnabledColumnCount", "EnabledColumnCountRME"))))
      val mmio_ColumnWidth = Seq((0x40) -> Seq(RegField(r_ColumnWidths.getWidth, r_ColumnWidths, RegFieldDesc(s"ColumnWidth", "ColumnWidth"))))
      val mmio_ColumnOffsets = r_ColumnOffsets.zipWithIndex.map {case (reg, i) => 
          (i * 0x10 + 0x48) -> Seq(RegField(reg.getWidth, reg, RegFieldDesc(s"ColumnOffset${i}", "ColumnOffset")))    
      }
      val mmio_FrameOffset = Seq((15 * 0x10 + 0x48) -> Seq(RegField(r_FrameOffset.getWidth, r_FrameOffset, RegFieldDesc("FrameOffset", "FrameOffset"))))
      val mmio_Reset = Seq((16 * 0x10 + 0x48) -> Seq(RegField(r_Reset.getWidth, r_Reset, RegFieldDesc("RMEReset", "RmeReset"))))
      val mmreg = mmio_Enable ++ mmio_RowSize ++ mmio_RowCount ++ mmio_EnabledColumnCount ++ 
                  mmio_ColumnWidth ++ mmio_ColumnOffsets ++ mmio_FrameOffset ++ mmio_Reset
      val regmap = ctlnode.regmap(mmreg: _*)







      val (out, out_edge) = node.out(i)
      val (in, in_edge) = node.in(i)
      val outParams = out_edge.bundle
      val inParams = in_edge.bundle

      val maxID = (math.pow(2, inParams.sourceBits)-1).toInt
      out.b <> in.b
      out.c <> in.c
      out.e <> in.e
      println(s"Client #$i Name: ${in_edge.client.clients(0).name}")
      println(s"source out bits ${out.a.bits.source.getWidth}\n")
      //val inDBeats = in_edge.numBeats(in.d.bits)
      //val demux = Module(new ConditionalDemuxA(inParams))
      //val rme_in_queue = Module(new Queue(new TLBundleA(inParams), 128, flow=false))
      //val rme_reply_queue = Module(new Queue(new TLBundleD(inParams), 128, flow=false))
      
      //val ConfigPort = new ConfigurationPortRME(params, device, i)
      val trapper = Module(new TrapperRME(params, in_edge, out_edge, in, i))
      val requestor = Module(new RequestorRME(params, in_edge, out_edge, out, i))
      val fetch_units : Vec[FetchUnitIO] = VecInit(Seq.tabulate(params.nFetchUnits) { j =>
        val fetch_unit = Module(new FetchUnitRME(params, node, in_edge, i, j))
        fetch_unit.io
      })
      val control_unit = Module(new ControlUnitRME(params, out_edge, out, i))
      val replyFromDRAMDemux = Module(new ConditionalDemuxD(out_edge.bundle))  
      //when (in.d.fire)
      //{
      //  SynthesizePrintf("in.d.fire\n")
      //}
      /*
        Input and output of RME
      */
      val isRMERequest = ToRME(in.a.bits.address) && (in.a.bits.opcode === TLMessages.Get) && config.Enabled
      val demux = Module(new ConditionalDemuxA(in_edge.bundle))
      demux.io.dataIn <> in.a
      demux.io.sel := isRMERequest
      trapper.io.TLInA <> demux.io.outB
      //trapper.io.TLInA.bits.address := UnmaskedAddress(demux.io.outB.bits.address)
      


      replyFromDRAMDemux.io.dataIn <> out.d
      //fetch_unit.io.inReply <> replyFromDRAMDemux.io.outB
      

      //when (in.a.fire)
      //{
      //  SynthesizePrintf(s"in.a.fire ${in.a.fire}\n")
      //}   
      //when (in.d.fire)
      //{
      //  SynthesizePrintf("in.d.fire\n")
      //}
//
      //when (out.a.fire)
      //{
      //  SynthesizePrintf("out.a.fire\n")
      //}
      //when (out.d.fire)
      //{
      //  SynthesizePrintf("out.d.fire\n")
      //}
      /*
        Fetch Unit broadcast 
      */
      // route back through RME for processing if fetch unit holds same source ID as the reply from DRAM
      val replySelectorCond = fetch_units.map{ fetch_unit => 
        val replySelector = fetch_unit.SrcId.valid && (fetch_unit.SrcId.bits === out.d.bits.source)
        replySelector
      }
      replyFromDRAMDemux.io.sel := replySelectorCond.reduce(_ || _) // if any conditions are true, broadcast to fetch units
      replyFromDRAMDemux.io.outB.ready := false.B // default 
      for (n <- 0 until fetch_units.length)
      {
        val fetch_unit = fetch_units(n)
        fetch_unit.inReply.valid := replySelectorCond(n)
        fetch_unit.inReply.bits := replyFromDRAMDemux.io.outB.bits
        when (replySelectorCond(n)) // when this fetch unit matches src ID, we fed that ready signal to demux
        {
          replyFromDRAMDemux.io.outB.ready := fetch_unit.inReply.ready // may need to set a default ready
        }
      }




      // Either from trapper or directly from DRAM if not an rme request
      TLArbiter.robin(in_edge, in.d, trapper.io.TLInD, replyFromDRAMDemux.io.outA)

      // Outgoing arbiter for passthrough and RME requests
      val fetch_unit_outbound = fetch_units.map(fetch_unit => fetch_unit.OutReq)
      TLArbiter.robin(out_edge, out.a, (Seq(demux.io.outA) ++ fetch_unit_outbound):_*) // we have to pass as a single Seq i guess
      


      /*
        Connections between RME modules
      */

      requestor.io.Trapper.Request.bits := trapper.io.Requestor.Request.bits
      requestor.io.Trapper.Request.valid := trapper.io.Requestor.Request.valid
      trapper.io.Requestor.Request.ready := requestor.io.Trapper.Request.ready


      trapper.io.ControlUnit <> control_unit.io.TrapperPort

      requestor.io.Config := config

      /*
        FetchUnit(s)/Requestor connection
      */
      val ohFetchUnitsReady = PriorityEncoderOH(fetch_units.map(fetch_unit => fetch_unit.Requestor.ready))
      requestor.io.FetchUnit.ready := ohFetchUnitsReady.reduce(_||_) // this should fire to the right one
      for (n <- 0 until fetch_units.length)
      {
          val fetch_unit = fetch_units(n)
          fetch_unit.Requestor.valid := ohFetchUnitsReady(n) && requestor.io.FetchUnit.valid
          fetch_unit.Requestor.bits := requestor.io.FetchUnit.bits

      }

      //val fetch_units_req_arb = Module(new RRArbiter(new RequestorFetchUnitPort(inParams), params.nFetchUnits))
      //fetch_units_req_arb.io.ou


      //fetch_unit.io.FetchReq.valid := requestor.io.FetchReq.valid
      //requestor.io.FetchReq.ready := fetch_unit.io.FetchReq.ready
      //fetch_unit.io.FetchReq.bits := requestor.io.FetchReq.bits
      //fetch_unit.io.isBaseRequest := requestor.io.isBaseRequest

      //fetch_unit.io.Requestor.valid := requestor.io.FetchUnit.valid
      //requestor.io.FetchUnit.ready := fetch_unit.io.Requestor.ready

      
      

      val ctrl_unit_arb = Module(new RRArbiter(FetchUnitControlPort(inParams, maxID), params.nFetchUnits))
      val fetch_unit_ctrl_io = VecInit(fetch_units.map(fetch_unit => fetch_unit.ControlUnit))
      ctrl_unit_arb.io.in <> fetch_unit_ctrl_io
      control_unit.io.FetchUnitPort <> ctrl_unit_arb.io.out
      requestor.io.ControlUnit <> control_unit.io.RequestorPort

      // we need some extra logic here before we can do this with an arbiter
      // we need to make sure that we aren't packing separate requests
      //control_unit.io.FetchUnitPort.bits := fetch_unit.io.ControlUnit.bits
      //control_unit.io.FetchUnitPort.valid := fetch_unit.io.ControlUnit.valid
      //fetch_unit.io.ControlUnit.ready := control_unit.io.FetchUnitPort.ready




      
    }

      


      //node.in.map{case (e, i) => println("client %s\n", i.params.)}
    



      

    
      
    

      
      //SynthesizePrintf("Received address %x\n", in.a.bits.address)

    
      //when (in.a.fire)
      //{
      //  SynthesizePrintf("Address: 0x%x\n", in.a.bits.address)
      //}
    

      
    }
    
    //TLArbiter.robin(out_edge, out.a, in.a, manager_in.a)

    //manager_in.d.bits := manager_in_edge.AccessAck(manager_in.a.bits, 0x6969.U)
    //manager_in.d.valid := manager_in_edge.done(manager_in.a)

  

}

trait CanHaveRME extends {
  val rme: Option[RME]
}

trait HasReservedAddressRange extends SimpleDevice {
  hasReservedRange = true
}


class WithRME() extends Config((site, here, up) => {
  case RMEKey => Some(RelMemParams())
})

trait CanHavePeripheryRME { this: BaseSubsystem =>
  private val portName = "dram-bru"
  val pbus = locateTLBusWrapper(PBUS)
  val mbus = locateTLBusWrapper(MBUS)

  val rme = p(RMEKey) match {
    case Some(params) => {
      pbus.coupleTo(portName) {
        mbus.rme.get.ctlnode := 
        TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _ }

      mbus.rme.get
    }
    case None => None
  }
}