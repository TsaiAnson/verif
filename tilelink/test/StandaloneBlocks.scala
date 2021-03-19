package verif

import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.subsystem.WithoutTLMonitors
import freechips.rocketchip.tilelink.TLRegisterNode
import sifive.blocks.inclusivecache.{CacheParameters, InclusiveCache, InclusiveCacheMicroParameters}

object DefaultTLParams {
  def slave: TLSlavePortParameters = TLSlavePortParameters.v1(
    Seq(
      TLSlaveParameters.v1( // TL-UH master
        address = Seq(AddressSet(0x0, 0xfff)),
        supportsGet = TransferSizes(1, 32),
        supportsPutFull = TransferSizes(1, 32),
        supportsPutPartial = TransferSizes(1, 32),
        supportsLogical = TransferSizes(1, 32),
        supportsArithmetic = TransferSizes(1, 32),
        supportsHint = TransferSizes(1, 32),
        regionType = RegionType.UNCACHED)
    ),
    beatBytes = 8)

  def master(name: String = "TLMasterPort", idRange: IdRange = IdRange(0,1)): TLMasterPortParameters = TLMasterPortParameters.v1(
    Seq(
      TLMasterParameters.v1(name = name, sourceId = idRange)
    ))

  // Temporary cache parameters
  def slaveCache: TLSlavePortParameters = TLSlavePortParameters.v1(
    Seq(
      TLSlaveParameters.v1(
        address = Seq(AddressSet(0x0, 0xfff)),
        supportsGet = TransferSizes(1, 32),
        supportsPutFull = TransferSizes(1, 32),
        supportsPutPartial = TransferSizes(1, 32),
        supportsLogical = TransferSizes(1, 32),
        supportsArithmetic = TransferSizes(1, 32),
        supportsHint = TransferSizes(1, 32),
        supportsAcquireB = TransferSizes(1, 32),
        supportsAcquireT = TransferSizes(1, 32),
        regionType = RegionType.UNCACHED
      )
    ),
    endSinkId = 1, beatBytes = 8)

  def masterCache: TLMasterPortParameters = TLMasterPortParameters.v1(
    Seq(
      TLMasterParameters.v1(
        name = "TestBundle",
        supportsProbe = TransferSizes(1, 32),
        supportsGet = TransferSizes(1, 32),
        supportsPutFull = TransferSizes(1, 32),
        supportsPutPartial = TransferSizes(1, 32),
        supportsLogical = TransferSizes(1, 32),
        supportsArithmetic = TransferSizes(1, 32),
        supportsHint = TransferSizes(1, 32)
      )
    ))
}

class TLRegBankStandalone(
  mPortParams: TLMasterPortParameters = DefaultTLParams.master(),
  concurrency: Int = 1
)(implicit p: Parameters = new WithoutTLMonitors) extends LazyModule {
  val device = new SimpleDevice("TLRegBankStandalone", Seq("regbank"))

  val regNode = TLRegisterNode(
    address = Seq(AddressSet(0x0, 0xfff)), // TODO: infer address set from beatBytes and size
    device = device,
    beatBytes = 8, // TODO: make beatBytes a parameter
    concurrency = concurrency)

  val bridge = BundleBridgeToTL(mPortParams)
  regNode := bridge
  val ioInNode = BundleBridgeSource(() => TLBundle(TLBundleParameters(mPortParams, regNode.edges.in.head.slave)))
  bridge := ioInNode
  val in = InModuleBody { ioInNode.makeIO() }

  lazy val module = new LazyModuleImp(this) {
    val regs = RegInit(VecInit(Seq.fill(64)(0.U(64.W))))

    val tuples = regs.zipWithIndex.map { case (reg, i) =>
      (0x00 + (i * 8)) -> Seq(RegField(64,reg)) // TODO: randomize types of reg fields
    }
    regNode.regmap(tuples :_*)
  }
}

class TLRAMNoModelStandalone (val mPortParams: TLMasterPortParameters = DefaultTLParams.master(),
    address: AddressSet = AddressSet(0x0, 0x1ff),
    beatBytes: Int = 8,
    cacheable: Boolean = false,
    atomics: Boolean = true,
    sramReg: Boolean = false,
    fragmenterMaxBytes: Int = 32
  ) (implicit p: Parameters = new WithoutTLMonitors) extends LazyModule {
  val ram  = LazyModule(new TLRAM(address, cacheable=cacheable, atomics=atomics, beatBytes=beatBytes, sramReg=sramReg))
  val frag = TLFragmenter(beatBytes, fragmenterMaxBytes)
  val buffer = TLBuffer(BufferParams.default)
  ram.node := frag := buffer

  val bridge = BundleBridgeToTL(mPortParams)
  buffer := bridge
  val ioInNode = BundleBridgeSource(() => TLBundle(TLBundleParameters(mPortParams, bridge.edges.out.head.slave)))
  bridge := ioInNode
  val in = InModuleBody { ioInNode.makeIO() }
  val sPortParams = bridge.edges.out.head.slave

  lazy val module = new LazyModuleImp(this) {}
}

class TLRAMStandalone (
  val mPortParams: TLMasterPortParameters = DefaultTLParams.master(),
  address: AddressSet = AddressSet(0x0, 0x1ff),
  beatBytes: Int = 8,
  cacheable: Boolean = false,
  atomics: Boolean = true,
  sramReg: Boolean = false,
  fragmenterMaxBytes: Int = 32
) (implicit p: Parameters = new WithoutTLMonitors) extends LazyModule {
  val model = LazyModule(new TLRAMModel("TLRAMModel")) // TODO: remove when checkers are mature
  val ram  = LazyModule(new TLRAM(address, cacheable=cacheable, atomics=atomics, beatBytes=beatBytes, sramReg=sramReg))
  val frag = TLFragmenter(beatBytes, fragmenterMaxBytes)
  val buffer = TLBuffer(BufferParams.default)
  ram.node := model.node := frag := buffer

  val bridge = BundleBridgeToTL(mPortParams)
  buffer := bridge
  val ioInNode = BundleBridgeSource(() => TLBundle(TLBundleParameters(mPortParams, bridge.edges.out.head.slave)))
  bridge := ioInNode
  val in = InModuleBody { ioInNode.makeIO() }
  val sPortParams = bridge.edges.out.head.slave

  lazy val module = new LazyModuleImp(this) {}
}

class XBarToRAMStandalone(implicit p: Parameters = new WithoutTLMonitors) extends LazyModule {
  val mPortParams = DefaultTLParams.master()
  val sPortParams = DefaultTLParams.slave
  val bParams= TLBundleParameters(mPortParams, sPortParams)

  val model = LazyModule(new TLRAMModel("TLRAMModelXbarSimple"))
  val ram  = LazyModule(new TLRAM(AddressSet(0x0, 0x1ff), cacheable = false, atomics = true, beatBytes = 8))
  val xbar = LazyModule(new TLXbar)

  ram.node := model.node := TLBuffer() := xbar.node
  val TLSlave = xbar.node

  // Standalone Connections
  val ioInNode = BundleBridgeSource(() => TLBundle(bParams))
  val in = InModuleBody { ioInNode.makeIO() }

  TLSlave :=
    BundleBridgeToTL(mPortParams) :=
    ioInNode

  lazy val module = new LazyModuleImp(this) {}
}

class XBarToMultiRAMStandalone(implicit p: Parameters = new WithoutTLMonitors) extends LazyModule {
  val mPortParams = DefaultTLParams.master()
  val sPortParams = DefaultTLParams.slave
  val bParams = TLBundleParameters(mPortParams, sPortParams)

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
  val ioInNode = BundleBridgeSource(() => TLBundle(bParams))
  val ioInNodeRef = BundleBridgeSource(() => TLBundle(bParams))
  val in = InModuleBody { ioInNode.makeIO() }
  val inRef = InModuleBody { ioInNodeRef.makeIO() }

  TLSlave :=
    BundleBridgeToTL(mPortParams) :=
    ioInNode
  TLReference :=
    BundleBridgeToTL(mPortParams) :=
    ioInNodeRef

  lazy val module = new LazyModuleImp(this) {}
}

// TL Multi-Master Xbar RAM Slave Node Standalone
class XbarToRAMMultiMasterStandalone(implicit p: Parameters = new WithoutTLMonitors) extends LazyModule {
  val mPortParams = Seq(
    DefaultTLParams.master("one", IdRange(0, 1)),
    DefaultTLParams.master("two", IdRange(1, 2))
  )
  val sPortParams = Seq(DefaultTLParams.slave, DefaultTLParams.slave)
  val bParams = (mPortParams zip sPortParams).map{ case (m, s) => TLBundleParameters(m, s)}

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
class L2Standalone(implicit p: Parameters = new WithoutTLMonitors) extends LazyModule {
  // First set of PortParams are L1, second set are DRAM
  val mPortParams = Seq(DefaultTLParams.masterCache, DefaultTLParams.master())
  val sPortParams = Seq(DefaultTLParams.slaveCache, DefaultTLParams.slave)
  val bParams = (mPortParams zip sPortParams).map{ case (m, s) => TLBundleParameters(m, s)}

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

class TLPatternPusherStandalone(txns: Seq[Pattern])(implicit p: Parameters = new WithoutTLMonitors) extends LazyModule  {
  val mPortParams = DefaultTLParams.master()
  val sPortParams = DefaultTLParams.slave
  val bParams = TLBundleParameters(mPortParams, sPortParams)

  val patternp = LazyModule(new TLPatternPusher("patternpusher", txns))

  // Standalone Connections
  val ioOutNode = BundleBridgeSink[TLBundle]()
  val out = InModuleBody { ioOutNode.makeIO() }

  ioOutNode :=
    TLToBundleBridge(sPortParams) :=
    patternp.node

  lazy val module = new LazyModuleImp(this) {
    val start = RegNext(1.B, 0.B)
    patternp.module.io.run := start
  }
}

class TLFuzzerStandalone(nOperations: Int)(implicit p: Parameters = new WithoutTLMonitors) extends LazyModule  {
  val mPortParams = DefaultTLParams.master()
  val sPortParams = DefaultTLParams.slave
  val bParams = TLBundleParameters(mPortParams, sPortParams)

  val tlfuzzer = LazyModule(new freechips.rocketchip.tilelink.TLFuzzer(nOperations, inFlight=1))

  // Standalone Connections
  val ioOutNode = BundleBridgeSink[TLBundle]()
  val out = InModuleBody { ioOutNode.makeIO() }

  ioOutNode :=
    TLToBundleBridge(sPortParams) :=
    tlfuzzer.node

  lazy val module = new LazyModuleImp(this) {}
}

class TLBufferStandalone(implicit p: Parameters = new WithoutTLMonitors) extends LazyModule  {
  val mPortParams = DefaultTLParams.master()
  val sPortParams = DefaultTLParams.slave
  val bParams = TLBundleParameters(mPortParams, sPortParams)

  val ioInNode = BundleBridgeSource(() => TLBundle(bParams))
  val ioOutNode = BundleBridgeSink[TLBundle]()
  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }

  ioOutNode :=
    TLToBundleBridge(sPortParams) :=
    TLBuffer() :=
    BundleBridgeToTL(mPortParams) :=
    ioInNode

  lazy val module = new LazyModuleImp(this) {}
}
