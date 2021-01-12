package designs

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink.TLRegisterNode
import sifive.blocks.inclusivecache.{CacheParameters, InclusiveCache, InclusiveCacheControlParameters, InclusiveCacheMicroParameters, InclusiveCacheParameters, InclusiveCachePortParameters}
import verif.TLUtils._

// Keeping as reference
trait VerifTLStandaloneBlock extends LazyModule {
  val masterParams = standaloneMasterParams
  val slaveParams = standaloneSlaveParams

  val ioInNode = BundleBridgeSource(() => TLBundle(verifTLBundleParams))
  val ioOutNode = BundleBridgeSink[TLBundle]()

  val TLMaster: TLOutwardNode
  val TLSlave: TLInwardNode

   ioOutNode :=
     TLToBundleBridge(standaloneSlaveParams) :=
     TLMaster

  TLSlave :=
    BundleBridgeToTL(standaloneMasterParams) :=
    ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }
}

class VerifTLRegBankSlave(implicit p: Parameters) extends LazyModule  {
  val device = new SimpleDevice("VerifTLRegBankSlave", Seq("veriftldriver,veriftlmonitor,testmaster")) // Not sure about compatibility list

  val TLSlave = TLRegisterNode(
    address = Seq(AddressSet(0x0, 0xfff)),
    device = device,
    beatBytes = 8,
    concurrency = 1)

  // Standalone Connections
  val ioInNode = BundleBridgeSource(() => TLBundle(verifTLBundleParams))
  val in = InModuleBody { ioInNode.makeIO() }

  TLSlave :=
    BundleBridgeToTL(standaloneMasterParams) :=
    ioInNode

  lazy val module = new LazyModuleImp(this) {
    val regs = RegInit(VecInit(Seq.fill(64)(0.U(64.W))))

    val tuples = regs.zipWithIndex.map { case (reg, i) =>
      (0x00 + (i * 8)) -> Seq(RegField(64,reg))
    }
    TLSlave.regmap(tuples :_*)
  }
}

class VerifTLRAMSlave(implicit p: Parameters) extends LazyModule {

  val model = LazyModule(new TLRAMModel("TLRAMModel"))
  val ram  = LazyModule(new TLRAM(AddressSet(0x0, 0x1ff), cacheable = false, atomics = true, beatBytes = 8))
  val frag = TLFragmenter(8, 32)
  val buffer = TLBuffer(BufferParams.default)
  ram.node := model.node := frag := buffer

  // Standalone Connections
  val ioInNode = BundleBridgeSource(() => TLBundle(verifTLBundleParams))
  val in = InModuleBody { ioInNode.makeIO() }

  buffer :=
    BundleBridgeToTL(standaloneMasterParams) :=
    ioInNode

  lazy val module = new LazyModuleImp(this) {}
}

class VerifTLXbarRAMSimpleSlave(implicit p: Parameters) extends LazyModule {

  val model = LazyModule(new TLRAMModel("TLRAMModelXbarSimple"))
  val ram  = LazyModule(new TLRAM(AddressSet(0x0, 0x1ff), cacheable = false, atomics = true, beatBytes = 8))
  val xbar = LazyModule(new TLXbar)

  ram.node := model.node := TLBuffer() := xbar.node
  val TLSlave = xbar.node

  // Standalone Connections
  val ioInNode = BundleBridgeSource(() => TLBundle(verifTLBundleParams))
  val in = InModuleBody { ioInNode.makeIO() }

  TLSlave :=
    BundleBridgeToTL(standaloneMasterParams) :=
    ioInNode

  lazy val module = new LazyModuleImp(this) {}
}

// TL Multi-Slave Xbar RAM Slave Node with References Standalone
class VerifTLMSXbarRAMSlaveReferenceStandalone(implicit p: Parameters) extends LazyModule {
  // Multi RAM
  val model1 = LazyModule(new TLRAMModel("TLRAMModel1"))
  val ram1  = LazyModule(new TLRAM(AddressSet(0x0, 0xff), cacheable = false, atomics = true, beatBytes = 8))
  val model2 = LazyModule(new TLRAMModel("TLRAMModel2"))
  val ram2  = LazyModule(new TLRAM(AddressSet(0x100, 0xff), cacheable = false, atomics = true, beatBytes = 8))
  val xbar = LazyModule(new TLXbar)
  ram1.node := model1.node := TLBuffer() := xbar.node
  ram2.node := model2.node := TLBuffer() := xbar.node
  val TLSlave = xbar.node

  // RAM Reference
  val model = LazyModule(new TLRAMModel("TLRAMModelReference"))
  val ram  = LazyModule(new TLRAM(AddressSet(0x0, 0x1ff), cacheable = false, atomics = true, beatBytes = 8))
  ram.node := model.node
  val TLReference = model.node

  // Connections
  val ioInNode = BundleBridgeSource(() => TLBundle(verifTLBundleParams))
  val ioInNodeRef = BundleBridgeSource(() => TLBundle(verifTLBundleParams))
  val in = InModuleBody { ioInNode.makeIO() }
  val inRef = InModuleBody { ioInNodeRef.makeIO() }

  TLSlave :=
    BundleBridgeToTL(standaloneMasterParams) :=
    ioInNode
  TLReference :=
    BundleBridgeToTL(standaloneMasterParams) :=
    ioInNodeRef

  lazy val module = new LazyModuleImp(this) {}
}

// TL Multi-Master Xbar RAM Slave Node Standalone
class VerifTLMMXbarRAMSlaveStandalone(implicit p: Parameters) extends LazyModule {
  def standaloneMasterParamsOne: TLMasterPortParameters = TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = "one", sourceId = IdRange(0,1))))
  def standaloneMasterParamsTwo: TLMasterPortParameters = TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = "two", sourceId = IdRange(1,2))))

  // TLRAM
  val model = LazyModule(new TLRAMModel("TLRAMModel"))
  val ram  = LazyModule(new TLRAM(AddressSet(0x0, 0x1ff), cacheable = false, atomics = true, beatBytes = 8))
  val xbar = LazyModule(new TLXbar)
  ram.node := model.node := TLBuffer() := xbar.node
  val TLReference = model.node

  // Connections
  val ioInNodeOne = BundleBridgeSource(() => TLBundle(TLBundleParameters(standaloneMasterParamsOne, standaloneSlaveParams)))
  val ioInNodeTwo = BundleBridgeSource(() => TLBundle(TLBundleParameters(standaloneMasterParamsTwo, standaloneSlaveParams)))
  val inOne = InModuleBody { ioInNodeOne.makeIO() }
  val inTwo = InModuleBody { ioInNodeTwo.makeIO() }

  xbar.node := TLBuffer() :=
    BundleBridgeToTL(standaloneMasterParamsOne) :=
    ioInNodeOne
  xbar.node := TLBuffer() :=
    BundleBridgeToTL(standaloneMasterParamsTwo) :=
    ioInNodeTwo

  lazy val module = new LazyModuleImp(this) {}
}

// L2 Cache Standalone
class VerifTLL2Cache(implicit p: Parameters) extends LazyModule {
  // Instantiating L2 Cache (Inclusive Cache)
  val l2 = LazyModule(new InclusiveCache(
    CacheParameters(
      level = 2,
      ways = 2,
      sets = 2,
      blockBytes = 32,
      beatBytes = 8),
    InclusiveCacheMicroParameters(writeBytes = 8),
    None
  ))
  val cork = LazyModule(new TLCacheCork)

  // IO Connections (Master and Slave are directly connected)
  val ioInNode = BundleBridgeSource(() => TLBundle(verifTLBundleParamsC))
  val ioOutNode = BundleBridgeSink[TLBundle]()
  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }

//  val ioCtrlNode = BundleBridgeSource(() => TLBundle(verifTLBundleParamsC))
//  val ctrl = InModuleBody { ioCtrlNode.makeIO() }

  ioOutNode :=
    TLToBundleBridge(standaloneSlaveParams) :=
    cork.node :=
    l2.node :=
    BundleBridgeToTL(standaloneMasterParamsC) :=
    ioInNode

//  l2.ctlnode := BundleBridgeToTL(standaloneMasterParamsC) := ioCtrlNode

  lazy val module = new LazyModuleImp(this) {}
}

// Example of manual Master
class VerifTLCustomMaster(implicit p: Parameters) extends LazyModule  {

  val TLMaster = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
    name = "testmaster",
    sourceId = IdRange(0,1),
    requestFifo = true,
    visibility = Seq(AddressSet(0x0, 0xfff)))))))

  // Standalone Connections
  val ioOutNode = BundleBridgeSink[TLBundle]()
  val out = InModuleBody { ioOutNode.makeIO() }

  ioOutNode :=
    TLToBundleBridge(standaloneSlaveParams) :=
    TLMaster

  lazy val module = new LazyModuleImp(this) {
    val (out, edge) = TLMaster.out(0)
    val addr = RegInit(UInt(8.W), 0.U)
    val response = RegInit(UInt(5.W), 0.U)
    val alt = RegInit(Bool(), false.B)

    // Behavior: Read in address x and then write result in address + 0x20.U
    // Operates on addresses from 0x0 to 0x18

    // Offset by 8 needed here, will look into later
    when (alt) {
      out.a.bits := edge.Get(0.U, addr - 0x8.U, 3.U)._2
    } otherwise {
      out.a.bits := edge.Put(0.U, addr + 0x18.U, 3.U, response)._2
    }

    when (out.a.fire()) {
      when (!alt) {
        addr := addr + 0x8.U
      }
      alt := !alt
    }

    when (out.d.valid) {
      response := out.d.deq().data + 10.U
    }

    // Hack to fix missing last instruction, fix later
    out.a.valid := addr < 0x20.U || (addr === 0x20.U && alt)
  }
}

class VerifTLMasterPattern(txns: Seq[Pattern])(implicit p: Parameters) extends LazyModule  {

  val patternp = LazyModule(new TLPatternPusher("patternpusher", txns))
  val TLMaster = patternp.node

  // Standalone Connections
  val ioOutNode = BundleBridgeSink[TLBundle]()
  val out = InModuleBody { ioOutNode.makeIO() }

  ioOutNode :=
    TLToBundleBridge(standaloneSlaveParams) :=
    TLMaster

  lazy val module = new LazyModuleImp(this) {
    val testIO = IO(new Bundle {
      val run = Input(Bool())
      val done = Output(Bool())
    })

    RegNext(patternp.module.io.run) := testIO.run
    testIO.done := patternp.module.io.done
  }
}

class VerifTLMasterFuzzer(implicit p: Parameters) extends LazyModule  {

  val TLMaster = TLFuzzer(30, inFlight=1)

  // Standalone Connections
  val ioOutNode = BundleBridgeSink[TLBundle]()
  val out = InModuleBody { ioOutNode.makeIO() }

  ioOutNode :=
    TLToBundleBridge(standaloneSlaveParams) :=
    TLMaster

  lazy val module = new LazyModuleImp(this) {}
}

// Connects Master and Slave Drivers together
class VerifTLMasterSlaveFeedback(implicit p: Parameters) extends LazyModule  {

  // IO Connections (Master and Slave are directly connected)
  val ioInNode = BundleBridgeSource(() => TLBundle(verifTLBundleParams))
  val ioOutNode = BundleBridgeSink[TLBundle]()
  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }

  ioOutNode :=
    TLToBundleBridge(standaloneSlaveParams) :=
    TLBuffer() :=
    BundleBridgeToTL(standaloneMasterParams) :=
    ioInNode

  lazy val module = new LazyModuleImp(this) {}
}
