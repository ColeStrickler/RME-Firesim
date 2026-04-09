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
import freechips.rocketchip.util.SeqToAugmentedSeq
import agu._
import _root_.subsystem.rme.subsystem.rme.{DTUCachedRegionManager, DTUUncachedRegion}

case class RelMemParams (
    regaddress: Int = 0x3000000,
    rmeaddress: BigInt =     0x170000000L,
    rmeAddressSize: BigInt =  0x10000000L,
    controlBeatBytes : Int = 8,
    nFetchUnits : Int = 16,
    inBoundXbar : Option[TLXbar] = None,
    withPerfCounter : Boolean = false,
    maxConfigs : Int = 8,
    maxDataSize : Int = 3, // 2^maxDataSize --> same as TL.A.size
)



case object RMEKey extends Field[Option[RelMemParams]](None)


class RME(params: RelMemParams)(implicit p: Parameters) extends LazyModule
{

    val addr = Seq(AddressSet(params.rmeaddress, params.rmeAddressSize-1))
    
    val device = new SimpleDevice("relmem",Seq("ku-csl,relmem")) with HasReservedAddressRange {
    
    }
    


    
    /*
      We need this to reserve an address range in the device tree 
      -> This required modifications to the device tree generation. See RocketChip fork
    */
    ResourceBinding {
      Resource(device, "reserved").bind(ResourceAddress(addr, rocketchip.resources.ResourcePermissions(true, true, false, false, true)))
    }

    
    val maxRMEOffsetBitWidth = log2Ceil(params.rmeAddressSize)
    
    
  val node = TLAdapterNode()

  val agu_vec = Seq.tabulate(params.maxConfigs) { i =>
    LazyModule(new AGUTop(new AGUParams2, i, maxRMEOffsetBitWidth))
  }


  

  val device2 = new SimpleDevice("dtu_region", Seq("dturegion")) with HasReservedAddressRange {

  }
  //val mdev = new MemoryDevice with HasReservedAddressRange
  

  val beatBytes = 8
  val maxDRAM = math.pow(2, 33).toLong
  val addr2 = AddressSet.misaligned(maxDRAM, (BigInt(1) << 47) - maxDRAM)
  // ResourceBinding {
  //  Resource(device2, "reserved").bind(ResourceAddress(addr2, rocketchip.resources.ResourcePermissions(true, true, false, true, true)))
  //}




  val dtu_cached_region = TLManagerNode(Seq(TLSlavePortParameters.v1(Seq(TLManagerParameters(
    address = addr2,
    regionType = RegionType.UNCACHED,
    //resources = mdev.reg,
    executable = true,
    supportsGet = TransferSizes(64, 64),
    supportsPutFull = TransferSizes(64, 64),
    supportsPutPartial = TransferSizes(64, 64),
    //supportsAcquireB = TransferSizes(64, 64), // cache cork should save us
    //supportsAcquireT = TransferSizes(64, 64),// cache cork should save us
    fifoId = Some(0))), beatBytes = beatBytes /*,endSinkId = 1*/ )))
 // val dtu_uncached_region = LazyModule(new DTUUncachedRegion)


    

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
    println(s"Number of edges into RME: $nClients\n")
    //require(nClients == 1)
   // val aguModule = agu.module  // hardware instance of AGUTop



    val config = Wire(RMEConfigPortIO(params))
     // Registers



        
        val r_RowSize = RegInit(0.U(32.W))
        val r_RowCount = RegInit(0.U(32.W))
        val r_EnabledColumnCount = RegInit(0.U(4.W))
        val r_ColumnWidths = RegInit(4.U(6.W)) // will change later
        val r_ColumnOffsets = RegInit(VecInit(Seq.fill(15)(0.U(7.W))))
        val r_FrameOffset = RegInit(0.U(32.W))
        val r_Reset = RegInit(false.B)
        val r_EnableRME = RegInit(false.B)


        val perfDTU = RegInit(0.U(64.W))
        val perfNonDTU = RegInit(0.U(64.W))



        val r_FetchFullStall =      if (params.withPerfCounter) Some(RegInit(0.U(64.W))) else None
        val r_FetchToCtrlStall =    if (params.withPerfCounter) Some(RegInit(0.U(64.W))) else None
        val r_FetchToMemoryStall =  if (params.withPerfCounter) Some(RegInit(0.U(64.W))) else None
        val r_CtrlToTrapperStall =  if (params.withPerfCounter) Some(RegInit(0.U(64.W))) else None
        val r_ReqDescFullStall =     if (params.withPerfCounter) Some(RegInit(0.U(64.W))) else None
        val r_EphemeralRegionConfig_Start = RegInit(VecInit(Seq.fill(params.maxConfigs)(params.rmeaddress.U(33.W))))
        val r_EphemeralRegionConfig_Size = RegInit(VecInit(Seq.fill(params.maxConfigs)(0.U(log2Ceil(params.rmeAddressSize).W))))
        val r_EphemeralRegionConfig_PhysStart = RegInit(VecInit(Seq.fill(params.maxConfigs)(0.U(47.W))))



        def CheckConfigHit(addr: UInt) : UInt = {
            //val hitIndex = Wire(0.U(log2Ceil(params.maxConfigs).W))
            val hits = (0 until params.maxConfigs).map { i =>
              val start = r_EphemeralRegionConfig_Start(i)
              val size  = r_EphemeralRegionConfig_Size(i)
              (addr >= start) && (addr < (start + size))
            }
            val numHits = PopCount(VecInit(hits)) // counts how many are true
            assert(numHits > 0.U, "Address matches less than one ephemeral region!")
            assert(numHits === 1.U, "Address matches more than one ephemeral region!")

            val hitIndex = PriorityEncoder(hits)
            hitIndex
        }

       println(s"params.withPerfCounter = ${params.withPerfCounter}")
       //require(params.withPerfCounter, "Performance counters must be enabled for this code to run.")

        
        val perfCounters =  if (params.withPerfCounter) {
        
          val stall_fetch_full = Seq((0xf00) -> Seq(RegField(r_FetchFullStall.get.getWidth, r_FetchFullStall.get, RegFieldDesc("FetchFullStall", "FetchFullStall"))))
          val stall_fetchToControl = Seq((0xf08) -> Seq(RegField(r_FetchToCtrlStall.get.getWidth, r_FetchToCtrlStall.get, RegFieldDesc("FetchToCtrlStall", "FetchToCtrlStall"))))
          val stall_fetchToMemory = Seq((0xf10) -> Seq(RegField(r_FetchToMemoryStall.get.getWidth, r_FetchToMemoryStall.get, RegFieldDesc("FetchToMemoryStall", "FetchToMemoryStall"))))
          val stall_CtrlToTrapper = Seq((0xf18) -> Seq(RegField(r_CtrlToTrapperStall.get.getWidth, r_CtrlToTrapperStall.get, RegFieldDesc("CtrlToTrapperStall", "CtrlToTrapperStall"))))
          val stall_reqToFetch = Seq((0xf20) -> Seq(RegField(r_ReqDescFullStall.get.getWidth, r_ReqDescFullStall.get, RegFieldDesc("ReqDescFullStall", "ReqDescFullStall"))))
          val ret = stall_fetch_full ++ stall_fetchToControl ++ stall_fetchToMemory ++ stall_CtrlToTrapper ++ stall_reqToFetch
          ret
        }
        else {
          Seq()
        }
                

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
         /*
                    
            val config_physStart = io.Config.EphemeralRegionConfig_PhysStart(matchedConfig)
            val config_size = io.Config.EphemeralRegionConfig_Size(matchedConfig)
            val EphemeralRegionConfig_Start = io.Config.EphemeralRegionConfig_Start(matchedConfig)


            From these we get the offset via --> offset = TLInA.bits.addr - config_physStart


            EphemeralRegionConfig_Start is the base of the data region that we update through. We use these so we can have an allocator
            split up the region. With the absolute offset we can calculate the offsets of the data pieces that and add those onto
            EphemeralRegionConfig_Start. 

            we can just simply pass in the matched config to the Requestor
        
        */


      val mmio_EphemeralConfigStart = r_EphemeralRegionConfig_Start.zipWithIndex.map {case (reg, i) => 
          (i * 0x8 + 0x400) -> Seq(RegField(reg.getWidth, reg, RegFieldDesc(s"r_EphemeralRegionConfig_Start${i}", "r_EphemeralRegionConfig_Start")))    
      }
      val mmio_EphemeralConfigSize  = r_EphemeralRegionConfig_Size.zipWithIndex.map {case (reg, i) => 
          (i * 0x8 + params.maxConfigs*0x8 + 0x400) -> Seq(RegField(reg.getWidth, reg, RegFieldDesc(s"r_EphemeralRegionConfig_Size${i}", "r_EphemeralRegionConfig_Size")))    
      }

      val mmio_EphemeralRegionConfig_PhysStart = r_EphemeralRegionConfig_PhysStart.zipWithIndex.map {case (reg, i) => 
          (i * 0x8 + 2*params.maxConfigs*0x8 + 0x400) -> Seq(RegField(reg.getWidth, reg, RegFieldDesc(s"r_EphemeralRegionConfig_PhysStart${i}", "r_EphemeralRegionConfig_PhysStart")))    
      }
      val dtu_access_reg = Seq((0xf00) -> Seq(RegField(perfDTU.getWidth, perfDTU, RegFieldDesc("perfDTU", "perfDTU"))))
      val nondtu_access_reg = Seq((0xf08) -> Seq(RegField(perfNonDTU.getWidth, perfNonDTU, RegFieldDesc("perfNonDTUl", "perfNonDTU"))))
          

     
      val mmreg = mmio_Enable ++ mmio_RowSize ++ mmio_RowCount ++ mmio_EnabledColumnCount ++ 
                  mmio_ColumnWidth ++ mmio_ColumnOffsets ++ mmio_FrameOffset ++ mmio_Reset ++ perfCounters ++
                  mmio_EphemeralConfigStart ++ mmio_EphemeralConfigSize ++ mmio_EphemeralRegionConfig_PhysStart ++ dtu_access_reg ++ nondtu_access_reg
      val regmap = ctlnode.regmap(mmreg: _*)

      config.RowSize := r_RowSize
      config.RowCount := r_RowCount
      config.EnabledColumnCount := r_EnabledColumnCount
      config.FrameOffset := r_FrameOffset
      config.ColumnWidths := r_ColumnWidths
      config.Enabled := r_EnableRME
      config.EphemeralRegionConfig_PhysStart := r_EphemeralRegionConfig_PhysStart
      config.EphemeralRegionConfig_Size := r_EphemeralRegionConfig_Size
      config.EphemeralRegionConfig_Start := r_EphemeralRegionConfig_Start

      for (i <- 0 until r_ColumnOffsets.length)
      {
          config.ColumnOffsets(i) := r_ColumnOffsets(i)
      }

      //SynthesizePrintf("rowsize: %d\n", r_RowSize)
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

    for (i <- 1 until nClients)
    {
      val (out, out_edge) = node.out(i)
      val (in, in_edge) = node.in(i)
      val outParams = out_edge.bundle
      val inParams = in_edge.bundle
      out <> in 
    }
        // Assign IO

        
      val i = 0

        //when (r_EnableRME)
        //{
        //    SynthesizePrintf("RME Enabled\n")
        //}
        


      println("MAPPING RME CONTROL REGISTERS")
      // MMIO Register Mapping
      
      val (out, out_edge) = node.out(i)
      val (in, in_edge) = node.in(i)
      val outParams = out_edge.bundle
      val inParams = in_edge.bundle

      val (cachedRegionIn, cachedRegionEdge) = dtu_cached_region.in(i)
      val cachedParams = cachedRegionEdge.bundle

      val inMaxID = (math.pow(2, cachedParams.sourceBits)-1).toInt
      val outMaxID = (math.pow(2, outParams.sourceBits)-1).toInt

      println(f"\n\ninMaxID ${inMaxID} ${cachedParams.sourceBits} outMaxID ${outMaxID} bits ${outParams.sourceBits} ${inParams.sourceBits}\n\n")

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
      val trapper = Module(new TrapperRME(params, cachedRegionEdge, out_edge, in, i))
      val requestors = VecInit(Seq.tabulate(params.maxConfigs){i => 
        val req = Module(new RequestorRME(params, cachedRegionEdge, out_edge, out, i))
        req.io
        })
      requestors.zipWithIndex.foreach{ case (req, i) =>
          req.agu <> agu_vec(i).module.io.reqIO
      }
      





      val fetch_units : Vec[FetchUnitIO] = VecInit(Seq.tabulate(params.nFetchUnits) { j =>
        val fetch_unit = Module(new FetchUnitRME(params, node, cachedRegionEdge, i, j))
        fetch_unit.io
      })


      val control_unit = Module(new ControlUnitRME(params, out_edge, cachedRegionEdge, i))
      val replyFromDRAMDemux = Module(new ConditionalDemuxD(out_edge.bundle))  
      //when (in.d.fire)
      //{
      //  SynthesizePrintf("in.d.fire\n")
      //}
      /*
        Input and output of RME
      */
      //val isRMERequest = ToRME(in.a.bits.address) && (in.a.bits.opcode === TLMessages.Get) && config.Enabled
      //val isWritebackToRME = ToRME(in.a.bits.address) && !(in.a.bits.opcode === TLMessages.Get)
      //val demux = Module(new toRMEConditionalDemuxA(in_edge.bundle, params))
      //demux.io.dataIn <> in.a
      //demux.io.sel := isRMERequest && !isWritebackToRME 
      //demux.io.isWriteback := false.B //isWritebackToRME && config.Enabled
      //trapper.io.TLInA <> demux.io.outB
      //trapper.io.TLInA.bits.address := UnmaskedAddress(demux.io.outB.bits.address)
      

      val dtu_cached_in_a = Wire(Decoupled(new TLBundleA(cachedParams)))
      when (cachedRegionIn.a.valid)
      {
       // SynthesizePrintf("cachedRegionIn.a.valid\n")
      }
      dtu_cached_in_a.bits := cachedRegionIn.a.bits
      dtu_cached_in_a.valid := cachedRegionIn.a.valid
      cachedRegionIn.a.ready := dtu_cached_in_a.ready
      dtu_cached_in_a.ready := trapper.io.TLInA.ready
      //dtu_cached_region.in(0)._1.a.ready := RegNext(dtu_cached_region.in(0)._1.a.valid) // dtu_cached_in_a.ready

      //when( dtu_cached_region.in(0)._1.a.valid)
      //{
      //  SynthesizePrintf(" dtu_cached_region.in(0)._1.a.valid 0x%x\n",  dtu_cached_region.in(0)._1.a.bits.address)
      //}


      trapper.io.Config := config
      trapper.io.TLInA <> dtu_cached_in_a
      cachedRegionIn.d <> trapper.io.TLInD



       in.d <> replyFromDRAMDemux.io.outA
  
      replyFromDRAMDemux.io.dataIn <> out.d
      //fetch_unit.io.inReply <> replyFromDRAMDemux.io.outB
     // SynthesizePrintf("Cycle");
   
      //when (in.a.fire)
      //{
      //  SynthesizePrintf(s"in.a.fire ${in.a.fire} 0x%x\n", in.a.bits.address)
      //}   

      
      //when (in.d.fire)
      //{
      //  SynthesizePrintf("in.d.fire\n")
      //}

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
        fetch_unit.inReply.valid := replySelectorCond(n) && out.d.valid
        fetch_unit.inReply.bits := replyFromDRAMDemux.io.outB.bits
        when (replySelectorCond(n)) // when this fetch unit matches src ID, we fed that ready signal to demux
        {
          replyFromDRAMDemux.io.outB.ready := fetch_unit.inReply.ready // may need to set a default ready
        }
      }

      //when (out.d.fire)
      //{
      //  SynthesizePrintf("dram resp %d\n", in.d.bits.source)
      //}


      perfDTU := perfDTU + fetch_units.map(fu => fu.OutReq.fire.asUInt).reduce(_ + _)
      perfNonDTU := perfNonDTU + in.a.fire


      // Either from trapper or directly from DRAM if not an rme request
      //TLArbiter.robin(in_edge, in.d, replyFromDRAMDemux.io.outA)
      in.d <> replyFromDRAMDemux.io.outA

      // Outgoing arbiter for passthrough and RME requests
      val fetch_unit_outbound = fetch_units.map(fetch_unit => fetch_unit.OutReq)
      TLArbiter.robin(out_edge, out.a, (Seq(in.a) ++ fetch_unit_outbound):_*) // we have to pass as a single Seq i guess
      
      if (params.withPerfCounter)
      {
        val fetchToMemStall = fetch_units.map(fetch_unit => fetch_unit.OutReq.valid).reduce(_||_) && 
          !fetch_units.map(fetch_unit => fetch_unit.OutReq.fire).reduce(_||_)

        when (fetchToMemStall)
        {
          //SynthesizePrintf("fetchToMemStall\n")
          r_FetchToMemoryStall.foreach{ reg=>
            reg := reg + 1.U
          }
        }
        
        
      }


      /*
        Connections between RME modules


        We can just use trapper.io.Requestor.trapperReq.bits.configMatch to route to the correct one
      */

      val config_hit = trapper.io.Requestor.trapperReq.bits.configMatch
      
      trapper.io.Requestor.trapperReq.ready := false.B
      requestors.zipWithIndex.foreach { case  (req, i) => 
        req.Trapper.trapperReq.valid := false.B // default
        req.Trapper.trapperReq.bits := Mux(config_hit === i.U, trapper.io.Requestor.trapperReq.bits, 0.U.asTypeOf(trapper.io.Requestor.trapperReq.bits))

        when (config_hit === i.U)
        {
          trapper.io.Requestor.trapperReq.ready := req.Trapper.trapperReq.ready
          req.Trapper.trapperReq.valid := trapper.io.Requestor.trapperReq.valid
        }

      }



      /*
        [TRAPPER := CONTROL UNIT]
      */
        trapper.io.ControlUnit <> control_unit.io.TrapperPort






      if (params.withPerfCounter)
      {
       // val ctrlToTrapperStall = control_unit.io.TrapperPort.valid && !control_unit.io.TrapperPort.fire
       // when (ctrlToTrapperStall)
       // {
       //   //SynthesizePrintf("ctrlToTrapperStall\n")
       //   r_CtrlToTrapperStall.foreach{ reg =>
       //     reg := reg + 1.U
       //   }
       // }
//
        
      }

      requestors.foreach { req =>
        req.Config := config
      }

      /*
        FetchUnit(s)/Requestor connection
      */

      /* 
        Requestor -> Fetch Unit

        [[Ticket prioritization logic]]
      */
      //val reqTicketVec = Wire(Vec(requestors.length, new ReqTicketInfo(requestors.length, 16)))
      //val reqTickets = requestors.zipWithIndex.map{case (req, i) =>
      //  (req.FetchUnit.bits.descriptor.ticket, i.U, req.FetchUnit.valid)  
      //}
      //reqTicketVec.zipWithIndex.foreach {case (reqT, i) =>
      //  reqT.ticket := reqTickets(i)._1
      //  reqT.index :=  reqTickets(i)._2
      //  reqT.valid := reqTickets(i)._3
      //}

      def ticketCompare(a: UInt, b: UInt): Bool = {
        val msbA = a(a.getWidth-1)
        val msbB = b(b.getWidth-1)

        Mux(msbA === msbB, a < b, msbA > msbB)
      }

      // basically we want to promote a ticket that is being packed...
      def srcIsBeingPacked(a: UInt, packingSrc: UInt) : Bool = {
          a === packingSrc
      } 

      val reqBeingPackedVec = Wire(Vec(requestors.length, Bool()))
      val reqBeingPackedExists = Wire(Bool())
      val reqBPacked = requestors.zipWithIndex.map{case (req, i) =>
          srcIsBeingPacked(req.FetchUnit.bits.BaseReq.source, control_unit.io.ID) && req.FetchUnit.valid && control_unit.io.useID
      }
      reqBeingPackedVec := reqBPacked
      reqBeingPackedExists := reqBeingPackedVec.reduce(_ || _)


      //val minReqTicket = reqTicketVec.reduceTree{ (a, b) => 
      //  val aBetter =
      //    a.valid && (
      //      !b.valid || ticketCompare(a.ticket, b.ticket)
      //    )
      //  Mux(aBetter, a, b)
      //}
      val reqFetchIO = requestors.map(_.FetchUnit)
      val reqdoneIO = requestors.map(_.FetchUnit.bits.descriptor.done)

      val fetchReadyVec = fetch_units.map(fetch_unit => fetch_unit.Requestor.ready)
      val ohFetchUnitsReady = PriorityEncoderOH(fetchReadyVec)
      val fetchUnitReady = fetchReadyVec.reduce(_||_) 
      

      val RequestorActive = RegInit(false.B)
      val ActiveRequestor = RegInit(0.U(log2Ceil(params.maxConfigs).W))
      val active_vector = reqFetchIO.map(req => req.fire)


      reqFetchIO.zipWithIndex.foreach {case (req, i) =>
          
          when (reqdoneIO(i) && req.fire && ActiveRequestor === i.U)
          {
            RequestorActive :=  false.B
          }
          .elsewhen (req.fire) 
          {
            ActiveRequestor := i.U
            RequestorActive := true.B
          }
      }
      
      // this should fire to the right one



      /*
        Before, I think we were swapping out the active request when it was still in the queue

      */
      val requestorArb = Module(new RRArbiter(requestors.head.FetchUnit.bits.cloneType, params.maxConfigs))
      requestorArb.io.in <> reqFetchIO
      reqFetchIO.zipWithIndex.foreach {case (req, i) => 
        req.ready := requestorArb.io.in(i).ready && (!RequestorActive || ActiveRequestor === i.U)
        requestorArb.io.in(i).valid := req.valid && (!RequestorActive || ActiveRequestor === i.U)
        when (reqBeingPackedVec(i))
        {
         // SynthesizePrintf("(CTRLFLOW) %d being packed\n", control_unit.io.ID)
        }
      }

      val selectedRequestor = requestorArb.io.out
      selectedRequestor.ready := fetchUnitReady 
      
      // CAN DEBUG WITH requestorArb.io.chosen
      for (n <- 0 until fetch_units.length)
      {
          val fetch_unit = fetch_units(n)
          fetch_unit.Requestor.valid := ohFetchUnitsReady(n) && selectedRequestor.valid
          fetch_unit.Requestor.bits := selectedRequestor.bits
      }


      val beatWidth = 8
      val dataRegWidth = (math.pow(2, params.maxDataSize+1)).toInt * beatWidth // this should give us the extra byte we need to extract excesses
      val fetch_unit_ctrl_io = VecInit(fetch_units.map(fetch_unit => fetch_unit.ControlUnit))
      when (control_unit.io.useID)
      {
          val valids = fetch_unit_ctrl_io.map(fu => fu.bits.baseReq.source === control_unit.io.ID && fu.valid)
          val selectedIdx = PriorityEncoder(valids.asUInt)
          control_unit.io.FetchUnitPort.valid := fetch_unit_ctrl_io(selectedIdx).valid
          control_unit.io.FetchUnitPort.bits  := fetch_unit_ctrl_io(selectedIdx).bits
          for ((fu, i) <- fetch_unit_ctrl_io.zipWithIndex) {
            fu.ready := (i.U === selectedIdx) && control_unit.io.FetchUnitPort.ready
          }
          //assert(valids.reduce(_ || _), "No fetch unit matches control_unit.io.ID")
      }
      .otherwise
      {
        
          val ctrl_unit_arb = Module(new RRArbiter(FetchUnitControlPort(cachedParams, inMaxID, outMaxID, dataRegWidth), params.nFetchUnits))
          ctrl_unit_arb.io.in <> fetch_unit_ctrl_io
          control_unit.io.FetchUnitPort <> ctrl_unit_arb.io.out
      }



      



      
      
      
      
      if (params.withPerfCounter)
      {
        // /val fetchToCtrlStall = fetch_units.map(fetch_unit => fetch_unit.ControlUnit.valid).reduce(_||_) && !control_unit.io.FetchUnitPort.fire
// /
// /
        // /when (fetchToCtrlStall)
        // /{
        // /  //SynthesizePrintf("fetchToCtrlStall\n")
        // /  r_FetchToCtrlStall.foreach{ reg =>
        // /    reg :=  reg + 1.U
        // /  }
        // /}
      }



      //for ((ctrl, i) <- fetch_unit_ctrl_io.zipWithIndex) {
      //  printf(p"FetchUnit $i: valid=${ctrl.valid}, ready=${ctrl.ready}\n")
      //}
      //printf(p"Arbiter Out: valid=${ctrl_unit_arb.io.out.valid}, ready=${ctrl_unit_arb.io.out.ready}\n")



      //requestors.zipWithIndex.foreach { case (req, i) =>
      //    req.ControlUnit <> control_unit.io.RequestorPort(i)
      //}
    




      
    

      


      //node.in.map{case (e, i) => println("client %s\n", i.params.)}
    



      

    
      
    

      
      //SynthesizePrintf("Received address %x\n", in.a.bits.address)

    
      //when (in.a.fire)
      //{
      //  SynthesizePrintf("Address: 0x%x\n", in.a.bits.address)
      //}
    

      
    }
    


  

}

trait CanHaveRME extends {
  val rme: Option[RME]
  val dtu_uncached_region : Option[DTUUncachedRegion]
}

trait HasReservedAddressRange extends Device {
  hasReservedRange = true
}



class WithRME() extends Config((site, here, up) => {
  case RMEKey => Some(RelMemParams())
})

trait CanHavePeripheryRME { this: BaseSubsystem =>
  private val portName = "dram-bru"
  val pbus = locateTLBusWrapper(PBUS)
  val mbus = locateTLBusWrapper(MBUS)
  val fbus = locateTLBusWrapper((FBUS))
  val sbus = locateTLBusWrapper(SBUS)


  val rme = p(RMEKey) match {
    case Some(params) => {
      pbus.coupleTo(portName) {
        mbus.rme.get.ctlnode := 
        TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _ }

      mbus.rme.get.agu_vec.foreach{ agu => 
        pbus.coupleTo(portName) {
          agu.ctlnode := 
          TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _ 
        }

      }
      
      mbus.coupleTo("dtu_cached_region") {
        mbus.rme.get.dtu_cached_region := TLFragmenter(mbus.beatBytes, mbus.blockBytes) := _
      }

    //val uncached = LazyModule(new DTUUncachedRegion)
      
    pbus.coupleTo("dtu_uncached") {
      mbus.dtu_uncached_region.get.cpuNode := 
      TLBuffer(1)  :=  _
    }
      // Connect uncached region to memory bus (MBUS) := TLFragmenter(mbus.beatBytes, mbus.blockBytes, holdFirstDeny=true)

    mbus.coupleFrom("simple_uncached_region_mem") { _ := mbus.dtu_uncached_region.get.memNode }
          
          
      mbus.rme.get
    }
    case None => None
}
  }
