package subsystem.rme

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
import freechips.rocketchip.tilelink.TLArbiter
import freechips.rocketchip.diplomacy.{AddressRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.subsystem.{BaseSubsystem, MBUS, Attachable}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.subsystem.Attachable





class IDAllocator(minID : Int, maxID : Int) extends Module {
  val io = IO(new Bundle {
    val newID = Decoupled(UInt(log2Ceil(maxID).W))
    val retireID = Flipped(Decoupled(UInt(log2Ceil(maxID).W)))
  })

  val queue_depth = math.min(maxID-minID+1, minID+64) // should always have room for retirement --> we don't check ready signal in ControlUnit
  val queue = Module(new Queue(UInt(log2Ceil(maxID).W), queue_depth))
  queue.io.enq <> io.retireID
  io.newID <> queue.io.deq

  


  val initDone = RegInit(false.B)
  val IDValues = VecInit((minID to maxID).map(i => i.U))
  println(f"IDAllocator allocating { ${minID} to ${maxID} }\n")


  val initCounter = RegInit(0.U(log2Ceil(queue_depth).W))


  when (!initDone)
  {
    queue.io.enq.bits := IDValues(initCounter)
    //SynthesizePrintf("IDAllocator init %d counter %d, %d\n", IDValues(initCounter), initCounter, IDValues.length.U)
    queue.io.enq.valid := true.B
    queue.io.deq.ready := false.B
    initCounter := Mux(queue.io.enq.fire, initCounter+1.U, initCounter)
    initDone := Mux(initCounter === (IDValues.length-1).U, true.B, false.B)
  }
}

/*
  Tile link diplomatic module that will increase the number of source IDs available to a downstream client.

  needed = new number of source IDs to accomodate
*/
class TLSourceExpander(needed: Int)(implicit p: Parameters)  extends LazyModule {
  def findBitsNeeded(baseWidth: Int, need: Int, check: Int) : Int = { // helper function to find bits needed
    //assert(check <= 3) // this shouldnt happen
    if (math.pow(2,baseWidth + check) - math.pow(2, baseWidth) >= need)
      check
    else
      findBitsNeeded(baseWidth, need, check+1)
  }
  
  
  val node = (new TLAdapterNode(
    clientFn  = { cp => 
      val baseWidth = log2Ceil(cp.endSourceId)
      val new_width = math.pow(2, baseWidth+needed).toInt
      val client = TLMasterParameters.v1(
        name     = "TLSourceExpander",
        sourceId = IdRange(0, new_width)
      )
      // We erase all client information since we crush the source Ids
      TLMasterPortParameters.v1(
        clients = Seq(client.v1copy(requestFifo = cp.clients.exists(_.requestFifo))),
        echoFields = cp.echoFields,
        requestFields = cp.requestFields,
        responseKeys = cp.responseKeys)
    },
    managerFn = { mp => mp.v1copy(managers = mp.managers.map(m => m.v1copy(fifoId = if (/*new_width==1*/false) Some(0) else m.fifoId))) // shouldn't happen
    }) {
    //override def circuitIdentity = edges.in.map(_.client).forall(noShrinkRequired)
  })


  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val len = node.in.length

    for (i <- 0 until len)
    {
      val (bundle, edge) = node.in(i)
      val (bundle_out, edge_out) = node.out(i)

      //when (bundle.a.valid)
      //{
      //  SynthesizePrintf("bundle.a.valid\n")
      //}
      bundle_out <> bundle
      //when (bundle_out.d.valid)
      //{
      //  SynthesizePrintf("bundle_out.d.valid\n")
      //  when (bundle.d.fire)
      //  {
      //    SynthesizePrintf("bundle.d.fire\n")
      //  }
      //}
    }
  }
}


object TLSourceExpander {
  def apply(needed: Int)(implicit p: Parameters) : TLSourceExpander = {
    val SourceExpander = LazyModule(new TLSourceExpander(needed))
    SourceExpander
  }
}





class ConditionalDemuxD(params: TLBundleParameters) extends Module {
  val io = IO(new Bundle {
    val dataIn = Flipped(DecoupledIO(new TLBundleD(params))) // Single input (8-bit)
    val sel    = Input(Bool())   // Selector (1-bit)
    val outA   = Decoupled(new TLBundleD(params))// Output to location A
    val outB   = Decoupled(new TLBundleD(params)) // Output to location B
  })

  // Default both outputs to zero
  val readyOther = Reg(Bool()) // so we have somewhere to connect it to

  val dummyMessage = Wire(new TLBundleD(params))
  dummyMessage.opcode := 0.U
  dummyMessage.param := 0.U
  dummyMessage.size := 0.U
  dummyMessage.source := 0.U
  dummyMessage.data := 0.U
  dummyMessage.denied := 0.U
  dummyMessage.sink := 0.U
  dummyMessage.corrupt := false.B


  // Route input based on selector
  when(io.sel) {
    //when (io.dataIn.fire)
    //{
    //  SynthesizePrintf("from DRAM back to RME src %d\n", io.dataIn.bits.source)
    //}
    
    io.outB <> io.dataIn
    io.outA.bits := dummyMessage
    io.outA.valid := false.B
    readyOther := io.outA.ready
  }.otherwise {
    //when (io.dataIn.fire)
    //{
    //  SynthesizePrintf("from DRAM skip RME src %d\n", io.dataIn.bits.source)
    //}
    
    io.outA <> io.dataIn
    io.outB.bits := dummyMessage
    io.outB.valid := false.B
    readyOther := io.outB.ready
  }
}


class ConditionalDemuxA(params: TLBundleParameters) extends Module {
  val io = IO(new Bundle {
    val dataIn = Flipped(DecoupledIO(new TLBundleA(params))) // Single input (8-bit)
    val sel    = Input(Bool())   // Selector (1-bit)
    val isWriteback = Input(Bool())
    val outA   = DecoupledIO(new TLBundleA(params))// Output to location A
    val outB   = DecoupledIO(new TLBundleA(params)) // Output to location B
  })

  // Default both outputs to zero
  val readyOther = Reg(Bool()) // so we have somewhere to connect it to

  val dummyMessage = Wire(new TLBundleA(params))
  dummyMessage.opcode := 0.U
  dummyMessage.param := 0.U
  dummyMessage.size := 0.U
  dummyMessage.source := 0.U
  dummyMessage.address := 0.U
  dummyMessage.mask := 0.U
  dummyMessage.data := 0.U
  dummyMessage.corrupt := false.B

  
  //when (io.sel)
  //{
  //  SynthesizePrintf("Selector = 1\n")
  //}



  // Route input based on selector
  when(io.sel) {
    io.outB <> io.dataIn
    io.outA.bits := dummyMessage
    io.outA.valid := false.B
    readyOther := io.outA.ready
    
  }.otherwise {
    io.outA <> io.dataIn
    io.outB.bits := dummyMessage
    io.outB.valid := false.B
    readyOther := io.outB.ready
  }
}

//
//class toRMEConditionalDemuxA(params: TLBundleParameters, rmeParams: RelMemParams) extends Module {
//  val io = IO(new Bundle {
//    val dataIn = Flipped(DecoupledIO(new TLBundleA(params))) // Single input (8-bit)
//    val sel    = Input(Bool())   // Selector (1-bit)
//    val isWriteback = Input(Bool())
//    val outA   = DecoupledIO(new TLBundleA(params))// Output to location A
//    val outB   = DecoupledIO(new TLBundleA(params)) // Output to location B
//  })
//
//  // Default both outputs to zero
//  val readyOther = Reg(Bool()) // so we have somewhere to connect it to
//
//  val dummyMessage = Wire(new TLBundleA(params))
//  dummyMessage.opcode := 0.U
//  dummyMessage.param := 0.U
//  dummyMessage.size := 0.U
//  dummyMessage.source := 0.U
//  dummyMessage.address := 0.U
//  dummyMessage.mask := 0.U
//  dummyMessage.data := 0.U
//  dummyMessage.corrupt := false.B
//
//  
//  //when (io.sel)
//  //{
//  //  SynthesizePrintf("Selector = 1\n")
//  //}
//
//
//
//  // Route input based on selector
//  when(io.sel) {
//    io.outB <> io.dataIn
//    io.outB.bits.address := io.dataIn.bits.address - (rmeParams.rmeShift).U
//    io.outA.bits := dummyMessage
//    io.outA.valid := false.B
//    readyOther := io.outA.ready
//  }.otherwise {
//    io.outA <> io.dataIn
//    io.outB.bits := dummyMessage
//    io.outB.valid := false.B
//    readyOther := io.outB.ready
//  }
//}
//
//
//
//
//
//
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import chisel3._

class DTUUncachedRegion(implicit p: Parameters) extends LazyModule {
  // Device info (optional, useful for reg/resource mapping)
  val device = new SimpleDevice("simpleForward", Seq("simple,forward"))
  val addr = AddressSet.misaligned(0x180000000L, 0x10000000L)


  val cpuNode = TLManagerNode(Seq(TLSlavePortParameters.v1(Seq(TLManagerParameters(
    address = addr,
    resources = device.reg,
    regionType = RegionType.UNCACHED,
    executable = false,
    supportsGet = TransferSizes(1, 64),
    supportsPutFull = TransferSizes(1, 64),
    supportsPutPartial = TransferSizes(1, 64),
    fifoId = Some(0))), 8))) // THIS MAY BE THE CAUSE OF BOOM CORE DIFFERENCES --> 16 TRANSFER SIZES INSTEAD OF 8 






  // 2. Outgoing to MBUS (master)
  val memNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLClientParameters(
    name = "SimpleForwardClient",
    sourceId = IdRange(0, 7)
  )))))
  //memNode := cpuNode
  lazy val module = new LazyModuleImp(this) {
     //Forward requests: cpuNode.in -> memNode.out
     val (inTL, inEdge) = cpuNode.in(0)   // From SBus
    val (outTL, outEdge) = memNode.out(0) // To MBUS

    // Register to hold the current request being forwarded
  val forwardReg = Reg(new TLBundleA(inTL.params))
  
  val id_allocator = Module(new IDAllocator(0, 7))
  val id = Reg(UInt(3.W))
  id_allocator.io.newID.ready := inTL.a.fire

  when (id_allocator.io.newID.fire)
  {
    id := id_allocator.io.newID.bits
  }

  println(s"inTL.A.params ${inTL.params}, outTL.a.params ${outTL.params}")
  

  id_allocator.io.retireID.valid := outTL.d.fire
  id_allocator.io.retireID.bits := id

  forwardReg := Mux(inTL.a.fire, inTL.a.bits, forwardReg)

  val (first, last, done, count, counter) = inEdge.firstlast2(inTL.a)

    val canSend = RegInit(false.B)
    val readyNewReq = RegInit(true.B)
    canSend := Mux(canSend, !outTL.a.fire, inTL.a.fire)
    readyNewReq := Mux(readyNewReq, !inTL.a.fire, inTL.d.fire)
    inTL.a.ready := readyNewReq

    outTL.a.bits := forwardReg          
    outTL.a.bits.source := id         
    outTL.a.bits.address := forwardReg.address -  0x10000000.U
    outTL.a.valid := canSend

    when(canSend)
    {
      //SynthesizePrintf("CANSEND\n")
    }

      when (inTL.a.valid )
      {
          //SynthesizePrintf("data 0x%x, size 0x%x\n", inTL.a.bits.data, inTL.a.bits.size)
        //assert(false.B, "!canSend && inTL.a.valid")
      }

    when (outTL.d.valid)
    {
     // SynthesizePrintf("Received back the request\n")
    }

    
    // Optional: forward D channel back
    inTL.d <> outTL.d 
    inTL.d.bits.source := forwardReg.source


  when (outTL.d.fire)
    {
      //SynthesizePrintf("Sending back the request 0x%x opcode 0x%x source: %d\n", outTL.d.bits.data, outTL.d.bits.opcode, forwardReg.source)
    }


    // Debug prints
    //when(inTL.a.fire) { SynthesizePrintf("[DTUUncachedRegion] got request id: %d\n", inTL.a.bits.source) }
   // when(outTL.a.fire) { SynthesizePrintf("[DTUUncachedRegion] forwarded request 0x%x\n", forwardReg.size) }
    when (outTL.a.fire)
    {
     // SynthesizePrintf("outTL.a.fire 0x%x %d --> data 0x%x\n", outTL.a.bits.address, outTL.a.bits.opcode, outTL.a.bits.data)
    }
  }
}


/*

class NTStoreRegion(implicit p: Parameters) extends LazyModule {
  // Device info (optional, useful for reg/resource mapping)
  val device = new SimpleDevice("simpleForward", Seq("simple,forward"))
  val addr = AddressSet.misaligned(0x180000000L, 0x10000000L)

  // Receives from CPU
  val cpuNode = TLManagerNode(Seq(TLSlavePortParameters.v1(Seq(TLManagerParameters(
    address = addr,
    resources = device.reg,
    regionType = RegionType.UNCACHED,
    executable = false,
    supportsGet = TransferSizes(1, 64),
    supportsPutFull = TransferSizes(1, 64),
    supportsPutPartial = TransferSizes(1, 64),
    fifoId = Some(0))), 8))) // THIS MAY BE THE CAUSE OF BOOM CORE DIFFERENCES --> 16 TRANSFER SIZES INSTEAD OF 8 



    val cacheClientNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLClientParameters(
      name = "NTStoreClient",
      sourceId = IdRange(0, 7)
    )))))


  // 2. Outgoing to MBUS (master)
  val memNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLClientParameters(
    name = "SimpleForwardClient",
    sourceId = IdRange(0, 7)
  )))))
  //memNode := cpuNode
  lazy val module = new LazyModuleImp(this) {
    //Forward requests: cpuNode.in -> memNode.out
    val (inTL, inEdge) = cpuNode.in(0)   // From SBus
    val (outTL, outEdge) = memNode.out(0) // To MBUS
    // Register to hold the current request being forwarded
    val forwardReg = Reg(new TLBundleA(inTL.params))
    
    val id_allocator = Module(new IDAllocator(0, 7))
    val id = Reg(UInt(3.W))
    id_allocator.io.newID.ready := inTL.a.fire
    when (id_allocator.io.newID.fire)
    {
      id := id_allocator.io.newID.bits
    }

    println(s"inTL.A.params ${inTL.params}, outTL.a.params ${outTL.params}")
    
    id_allocator.io.retireID.valid := outTL.d.fire
    id_allocator.io.retireID.bits := id
    forwardReg := Mux(inTL.a.fire, inTL.a.bits, forwardReg)
    val (first, last, done, count, counter) = inEdge.firstlast2(inTL.a)
    val canSend = RegInit(false.B)
    val readyNewReq = RegInit(true.B)
    canSend := Mux(canSend, !outTL.a.fire, inTL.a.fire)
    readyNewReq := Mux(readyNewReq, !inTL.a.fire, inTL.d.fire)
    inTL.a.ready := readyNewReq
    outTL.a.bits := forwardReg          
    outTL.a.bits.source := id         
    outTL.a.bits.address := forwardReg.address -  0x10000000.U
    outTL.a.valid := canSend




  val dataReg = RegInit(0.U(512.W))
  val dataRegWriteIdx = RegInit(0.U(10.W))
  
  /*
    Handle Communication with cache
  */
  val (outTLCache, cacheOutEdge) = cacheClientNode.out(0) // To SBUS
  val startID = cacheOutEdge.client.clients.head.sourceId.start

  val acquire :: sent_acquire :: update :: merge :: release :: idle :: Nil = Enum(6)
  val stateReg = RegInit(idle)

  val busWidth = outTLCache.a.bits.data.getWidth
  val dataregWriteIdxMax = 512/busWidth
  val nReqSent = RegInit(0.U(dataregWriteIdxMax.U))
  switch (stateReg)
  {
    is (idle) {

      dataRegWriteIdx := 0.U
      // Not sure if this is the correct one?? Either way we need something like this
      val PermReq  = cacheOutEdge.AcquireBlock(startID.U, forwardReg.address -  0x10000000.U, 6.U, TLPermissions.toT)
      outTLCache.a.bits := PermReq._2

      /*
          The L2 will handle requests from this client a bit differently. 
      
          If not present in the directory, it will just drop it because nothing needs to be done
      */




    }


    is (sent_acquire) { // state for waiting on reply to acquire
      when (outTLCache.d.fire && outTLCache.d.bits.opcode === TLMessages.GrantData) // Block is in Cache, we must update
      {
        // Do we need a Grant ACK??
        dataReg := Cat(outTLCache.d.bits.data, (dataReg >> busWidth)((dataReg.getWidth - 1)-busWidth, 0))
        dataRegWriteIdx := dataRegWriteIdx + 1.U
        when (dataRegWriteIdx === (dataregWriteIdxMax-1).U)
        {
          stateReg := update
        }
      }


      when (outTLCache.d.fire && outTLCache.d.bits.opcode === TLMessages.Grant) // Block not in cache, ignore
      {
          stateReg := idle
      } 


    }


    is (merge) {
        val putAddr = forwardReg.address -  0x10000000.U
        val offsetIntoLine = putAddr(5, 0) //putAddr % 0x40.U


        dataReg := (dataReg | mask) & shiftedData

      
    }

    is (update) { // here we actually write the data, then write it back 


      val putAddr = forwardReg.address -  0x10000000.U
      val DataWire = Wire(0.U(busWidth.W))

      /* 
        Need to hook up DataWire to dataReg
      */

      val putFullReq = cacheOutEdge.Put(startID.U, putAddr, 6.U, DataWire) // used to write back the data
      outTLCache.a.bits := putFullReq
      outTLCache.a.valid := true.B
      when (outTLCache.a.fire)
      {
        nReqSent := nReqSent + 1.U
      }



      when (nReqSent == (dataregWriteIdxMax-1).U && outTLCache.a.fire)
      {
        stateReg := release
      }

    }

    is (release) {
      val releaseAddr = forwardReg.address -  0x10000000.U
      val ReleaseReq = cacheOutEdge.Release(startID.U, releaseAddr, 6.U, TLPermissions.toN)

      outTLCache.c.bits := ReleaseReq._2
      outTLCache.c.valid := true.B
      when (outTLCache.c.fire)
      {
        stateReg := idle
      }

    }

  }


  }
}


*/



class DTUCachedRegionManager(implicit p: Parameters) extends LazyModule {
  val device = new SimpleDevice("dturegion", Seq("dtu,region"))
  val beatBytes = 8
  val maxDRAM = math.pow(2, 33).toLong
  val addr = AddressSet.misaligned(maxDRAM, (BigInt(1) << 47) - maxDRAM)
  println(addr)
  val node = TLManagerNode(Seq(TLSlavePortParameters.v1(Seq(TLManagerParameters(
    address = addr,
    resources = device.reg,
    regionType = RegionType.UNCACHED,
    executable = false,
    supportsGet = TransferSizes(64, 64),
    supportsPutFull = TransferSizes(64, 64),
    supportsPutPartial = TransferSizes(64, 64),
    fifoId = Some(0))), beatBytes)))

  lazy val module = new LazyModuleImp(this) {
    
    val (tl, edge) = node.in(0)
    when (tl.a.fire)
    {
    //  SynthesizePrintf("DTUCACHEDREGIONMANAGER FIRE\n");
    }
    
    val tlInParams = tl.params
    val currentlyBeating = RegInit(false.B)
    val currentRequest = Wire(Decoupled(new TLBundleD(tlInParams)))

    val inAReq = Reg(new TLBundleA(tlInParams))
    inAReq := Mux(tl.a.fire, tl.a.bits, inAReq)

    currentRequest.bits := edge.AccessAck(inAReq, 0x6969.U)
    currentRequest.valid := currentlyBeating
    val (d_first, d_last, d_done, beatCount, count) = edge.firstlast2(currentRequest)
    currentlyBeating := Mux(currentlyBeating, !d_done, tl.a.fire)

  


    tl.d <> currentRequest
    tl.a.ready := !currentlyBeating
    when (tl.d.fire)
    {
     // SynthesizePrintf("[DTUCachedRegionManager] ==> sent reply to 0x%x with data: 0x%x\n", inAReq.address, currentRequest.bits.data)
    }
    //assert(!tl.a.valid)
  }
}





case class ScratchPadIO(MemDepth: Int, MemWidth: Int) extends Bundle
{
  val enable = Input(Bool())
  val write = Input(Bool())
  val addr = Input(UInt(log2Ceil(MemDepth).W))
  val dataIn = Input(UInt(MemWidth.W))
  val dataOut = Output(UInt(MemWidth.W))
}



class ScratchPadMemBank(MemDepth: Int, MemWidth: Int) extends Module {
  val io = IO(new ScratchPadIO(MemDepth, MemWidth))



  // ensure address not out of range
  assert(io.addr < MemDepth.U)

  // should be synthesized as SRAM
  val mem = SyncReadMem(MemDepth, UInt(MemWidth.W))
  
  
  // Create one write port and one read port
  mem.write(io.addr, io.dataIn)
  io.dataOut := mem.read(io.addr, io.enable)
}