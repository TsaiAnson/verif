package verif

import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink.TLRegisterNode
import sifive.blocks.inclusivecache.{CacheParameters, InclusiveCache, InclusiveCacheMicroParameters}
import verif.TLUtils._

// Keeping as reference
trait VerifTLStandaloneBlock extends LazyModule {
  val masterParams = defaultStandaloneMasterParams
  val slaveParams = defaultStandaloneSlaveParams

  val ioInNode = BundleBridgeSource(() => TLBundle(defaultVerifTLBundleParams))
  val ioOutNode = BundleBridgeSink[TLBundle]()

  val TLMaster: TLOutwardNode
  val TLSlave: TLInwardNode

   ioOutNode :=
     TLToBundleBridge(defaultStandaloneSlaveParams) :=
     TLMaster

  TLSlave :=
    BundleBridgeToTL(defaultStandaloneMasterParams) :=
    ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }
}

class TLRegBankStandalone(implicit p: Parameters) extends LazyModule  {
  val device = new SimpleDevice("TLRegBankStandalone", Seq("veriftldriver,veriftlmonitor,testmaster"))
  val mPortParams: Seq[TLMasterPortParameters] = Seq(defaultStandaloneMasterParams)
  val sPortParams: Seq[TLSlavePortParameters] = Seq(defaultStandaloneSlaveParams)
  val bParams= (mPortParams zip sPortParams).map({t => TLBundleParameters(t._1, t._2)})

  val TLSlave = TLRegisterNode(
    address = Seq(AddressSet(0x0, 0xfff)),
    device = device,
    beatBytes = 8,
    concurrency = 1)

  // Standalone Connections
  val ioInNode = BundleBridgeSource(() => TLBundle(bParams.head))
  val in = InModuleBody { ioInNode.makeIO() }

  TLSlave :=
    BundleBridgeToTL(mPortParams.head) :=
    ioInNode

  lazy val module = new LazyModuleImp(this) {
    val regs = RegInit(VecInit(Seq.fill(64)(0.U(64.W))))

    val tuples = regs.zipWithIndex.map { case (reg, i) =>
      (0x00 + (i * 8)) -> Seq(RegField(64,reg))
    }
    TLSlave.regmap(tuples :_*)
  }
}

class TLRAMStandalone(implicit p: Parameters) extends LazyModule {
  val mPortParams: Seq[TLMasterPortParameters] = Seq(defaultStandaloneMasterParams)
  val sPortParams: Seq[TLSlavePortParameters] = Seq(defaultStandaloneSlaveParams)
  val bParams= (mPortParams zip sPortParams).map({t => TLBundleParameters(t._1, t._2)})

  val model = LazyModule(new TLRAMModel("TLRAMModel"))
  val ram  = LazyModule(new TLRAM(AddressSet(0x0, 0x1ff), cacheable = false, atomics = true, beatBytes = 8))
  val frag = TLFragmenter(8, 32)
  val buffer = TLBuffer(BufferParams.default)
  ram.node := model.node := frag := buffer

  // Standalone Connections
  val ioInNode = BundleBridgeSource(() => TLBundle(bParams.head))
  val in = InModuleBody { ioInNode.makeIO() }

  buffer :=
    BundleBridgeToTL(mPortParams.head) :=
    ioInNode

  lazy val module = new LazyModuleImp(this) {}
}

class XBarToRAMStandalone(implicit p: Parameters) extends LazyModule {
  val mPortParams: Seq[TLMasterPortParameters] = Seq(defaultStandaloneMasterParams)
  val sPortParams: Seq[TLSlavePortParameters] = Seq(defaultStandaloneSlaveParams)
  val bParams= (mPortParams zip sPortParams).map({t => TLBundleParameters(t._1, t._2)})

  val model = LazyModule(new TLRAMModel("TLRAMModelXbarSimple"))
  val ram  = LazyModule(new TLRAM(AddressSet(0x0, 0x1ff), cacheable = false, atomics = true, beatBytes = 8))
  val xbar = LazyModule(new TLXbar)

  ram.node := model.node := TLBuffer() := xbar.node
  val TLSlave = xbar.node

  // Standalone Connections
  val ioInNode = BundleBridgeSource(() => TLBundle(bParams.head))
  val in = InModuleBody { ioInNode.makeIO() }

  TLSlave :=
    BundleBridgeToTL(mPortParams.head) :=
    ioInNode

  lazy val module = new LazyModuleImp(this) {}
}

// TL Multi-Slave Xbar RAM Slave Node with References Standalone
class VerifTLMSXbarRAMSlaveReferenceStandalone(implicit p: Parameters) extends LazyModule {
  val mPortParams: Seq[TLMasterPortParameters] = Seq(defaultStandaloneMasterParams, defaultStandaloneMasterParams)
  val sPortParams: Seq[TLSlavePortParameters] = Seq(defaultStandaloneSlaveParams, defaultStandaloneSlaveParamsCache)
  val bParams= (mPortParams zip sPortParams).map({t => TLBundleParameters(t._1, t._2)})

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
  val ioInNode = BundleBridgeSource(() => TLBundle(bParams(0)))
  val ioInNodeRef = BundleBridgeSource(() => TLBundle(bParams(1)))
  val in = InModuleBody { ioInNode.makeIO() }
  val inRef = InModuleBody { ioInNodeRef.makeIO() }

  TLSlave :=
    BundleBridgeToTL(mPortParams(0)) :=
    ioInNode
  TLReference :=
    BundleBridgeToTL(mPortParams(1)) :=
    ioInNodeRef

  lazy val module = new LazyModuleImp(this) {}
}

// TL Multi-Master Xbar RAM Slave Node Standalone
class VerifTLMMXbarRAMSlaveStandalone(implicit p: Parameters) extends LazyModule {
  val mPortParams: Seq[TLMasterPortParameters] = Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = "one", sourceId = IdRange(0,1)))),
    TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = "two", sourceId = IdRange(1,2)))))
  val sPortParams: Seq[TLSlavePortParameters] = Seq(defaultStandaloneSlaveParams, defaultStandaloneSlaveParams)
  val bParams= (mPortParams zip sPortParams).map({t => TLBundleParameters(t._1, t._2)})

  // TLRAM
  val model = LazyModule(new TLRAMModel("TLRAMModel"))
  val ram  = LazyModule(new TLRAM(AddressSet(0x0, 0x1ff), cacheable = false, atomics = true, beatBytes = 8))
  val xbar = LazyModule(new TLXbar)
  ram.node := model.node := TLBuffer() := xbar.node
  val TLReference = model.node

  // Connections
  val ioInNodeOne = BundleBridgeSource(() => TLBundle(bParams(0)))
  val ioInNodeTwo = BundleBridgeSource(() => TLBundle(bParams(1)))
  val inOne = InModuleBody { ioInNodeOne.makeIO() }
  val inTwo = InModuleBody { ioInNodeTwo.makeIO() }

  xbar.node := TLBuffer() :=
    BundleBridgeToTL(mPortParams(0)) :=
    ioInNodeOne
  xbar.node := TLBuffer() :=
    BundleBridgeToTL(mPortParams(1)) :=
    ioInNodeTwo

  lazy val module = new LazyModuleImp(this) {}
}

// L2 Cache Standalone
class VerifTLL2Cache(implicit p: Parameters) extends LazyModule {
  // First set of PortParams are L1, second set are DRAM
  val mPortParams: Seq[TLMasterPortParameters] = Seq(defaultStandaloneMasterParamsCache, defaultStandaloneMasterParams)
  val sPortParams: Seq[TLSlavePortParameters] = Seq(defaultStandaloneSlaveParamsCache, defaultStandaloneSlaveParams)
  val bParams= (mPortParams zip sPortParams).map({t => TLBundleParameters(t._1, t._2)})

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
  val ioInNode = BundleBridgeSource(() => TLBundle(bParams(0)))
  val ioOutNode = BundleBridgeSink[TLBundle]()
  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }

//  val ioCtrlNode = BundleBridgeSource(() => TLBundle(verifTLBundleParamsC))
//  val ctrl = InModuleBody { ioCtrlNode.makeIO() }

  ioOutNode :=
    TLToBundleBridge(sPortParams(1)) :=
    cork.node :=
    l2.node :=
    BundleBridgeToTL(mPortParams(0)) :=
    ioInNode

//  l2.ctlnode := BundleBridgeToTL(standaloneMasterParamsC) := ioCtrlNode

  lazy val module = new LazyModuleImp(this) {}
}

class TLPatternPusherStandalone(txns: Seq[Pattern])(implicit p: Parameters) extends LazyModule  {
  val mPortParams: Seq[TLMasterPortParameters] = Seq(defaultStandaloneMasterParams)
  val sPortParams: Seq[TLSlavePortParameters] = Seq(defaultStandaloneSlaveParams)
  val bParams= (mPortParams zip sPortParams).map({t => TLBundleParameters(t._1, t._2)})


  val patternp = LazyModule(new TLPatternPusher("patternpusher", txns))
  val TLMaster = patternp.node

  // Standalone Connections
  val ioOutNode = BundleBridgeSink[TLBundle]()
  val out = InModuleBody { ioOutNode.makeIO() }

  ioOutNode :=
    TLToBundleBridge(sPortParams.head) :=
    TLMaster

  lazy val module = new LazyModuleImp(this) {
    val start = RegNext(1.B, 0.B)
    patternp.module.io.run := start
  }
}

class TLFuzzerStandalone(nOperations: Int)(implicit p: Parameters) extends LazyModule  {
  val mPortParams: Seq[TLMasterPortParameters] = Seq(defaultStandaloneMasterParams)
  val sPortParams: Seq[TLSlavePortParameters] = Seq(defaultStandaloneSlaveParams)
  val bParams= (mPortParams zip sPortParams).map({t => TLBundleParameters(t._1, t._2)})

  val tlfuzzer = LazyModule(new freechips.rocketchip.tilelink.TLFuzzer(nOperations, inFlight=1))
  val TLMaster = tlfuzzer.node

  // Standalone Connections
  val ioOutNode = BundleBridgeSink[TLBundle]()
  val out = InModuleBody { ioOutNode.makeIO() }

  ioOutNode :=
    TLToBundleBridge(sPortParams.head) :=
    TLMaster

  lazy val module = new LazyModuleImp(this) {}
}

class TLBufferStandalone(implicit p: Parameters) extends LazyModule  {
  val mPortParams: Seq[TLMasterPortParameters] = Seq(defaultStandaloneMasterParamsCache)
  val sPortParams: Seq[TLSlavePortParameters] = Seq(defaultStandaloneSlaveParamsCache)
  val bParams= (mPortParams zip sPortParams).map({t => TLBundleParameters(t._1, t._2)})

  val ioInNode = BundleBridgeSource(() => TLBundle(bParams.head))
  val ioOutNode = BundleBridgeSink[TLBundle]()
  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }

  ioOutNode :=
    TLToBundleBridge(sPortParams.head) :=
    TLBuffer() :=
    BundleBridgeToTL(mPortParams.head) :=
    ioInNode

  lazy val module = new LazyModuleImp(this) {}
}
