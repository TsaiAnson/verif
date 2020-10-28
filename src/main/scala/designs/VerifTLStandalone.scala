package designs

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile.{TileVisibilityNodeKey}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink.TLRegisterNode
import verif.VerifTLBase

trait VerifTLStandaloneBlock extends LazyModule with VerifTLBase {
  // Commented out for now
//  //Diplomatic node for mem interface (OPTIONAL)
//  val mem: Option[MixedNode[TLClientPortParameters, TLManagerPortParameters, TLEdgeIn, TLBundle,
//    TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLBundle]]
//
//  val ioMem = mem.map { m => {
//    val ioMemNode = BundleBridgeSource(() => TLBundle(standaloneParams))
//    m :=
//      BundleBridgeToTL(TLClientPortParameters(Seq(TLClientParameters("bundleBridgeToTL")))) :=
//      ioMemNode
//    val ioMem = InModuleBody { ioMemNode.makeIO() }
//    ioMem
//  }}

  val ioInNode = BundleBridgeSource(() => TLBundle(verifTLUBundleParams))
  val ioOutNode = BundleBridgeSink[TLBundle]()

  val TLClient: TLOutwardNode
  val TLManager: TLInwardNode

   ioOutNode :=
     TLToBundleBridge(TLManagerPortParameters(Seq(TLManagerParameters(address = Seq(AddressSet(0x0, 0xfff)),
       supportsGet = TransferSizes(1, 8), supportsPutFull = TransferSizes(1,8))), 16)) :=
     TLClient

  TLManager :=
    BundleBridgeToTL(TLClientPortParameters(Seq(TLClientParameters("bundleBridgeToTL")))) :=
    ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }
}

trait VerifRoCCStandaloneBlock extends LazyModule with VerifTLBase {
  val ioOutNode = BundleBridgeSink[TLBundle]()

  val tlNode: TLIdentityNode

  ioOutNode :=
    TLToBundleBridge(TLManagerPortParameters(Seq(TLManagerParameters(address = Seq(AddressSet(0x0, 0xfff)),
      supportsGet = TransferSizes(1, 64), supportsPutFull = TransferSizes(1,64), supportsPutPartial = TransferSizes(1,64))), 16)) :=
    tlNode

  val out = InModuleBody { ioOutNode.makeIO() }
}

class VerifTLRegBankManager(implicit p: Parameters) extends LazyModule  {
  val device = new SimpleDevice("VerifTLRegBankManager", Seq("veriftldriver,veriftlmonitor,testclient")) // Not sure about compatibility list

  val TLManager = TLRegisterNode(
    address = Seq(AddressSet(0x0, 0xfff)),
    device = device,
    beatBytes = 8,
    concurrency = 1)

  // Filler for now
  val TLClient = TLClientNode(Seq(TLClientPortParameters(Seq(TLClientParameters(
    name = "testclient",
    sourceId = IdRange(0,1),
    requestFifo = true,
    visibility = Seq(AddressSet(0x1000, 0xfff)))))))

  lazy val module = new LazyModuleImp(this) {
    val bigReg1 = RegInit(10.U(64.W))
    val bigReg2 = RegInit(11.U(64.W))
    val bigReg3 = RegInit(12.U(64.W))
    val bigReg4 = RegInit(13.U(64.W))

    // Will try to implement hardware FIFO (using Queue) for later examples
    TLManager.regmap(
      0x00 -> Seq(RegField(64, bigReg1)),
      0x08 -> Seq(RegField(64, bigReg2)),
      0x10 -> Seq(RegField(64, bigReg3)),
      0x18 -> Seq(RegField(64, bigReg4))
    )
  }
}

// Example of manual Client
class VerifTLCustomClient(implicit p: Parameters) extends LazyModule  {
  val device = new SimpleDevice("VerifTLCustomClient", Seq("veriftldriver,veriftlmonitor,testclient"))

  // Filler for now
  val TLManager = TLRegisterNode(
    address = Seq(AddressSet(0x0, 0xfff)),
    device = device,
    beatBytes = 8,
    concurrency = 1)

    val TLClient = TLClientNode(Seq(TLClientPortParameters(Seq(TLClientParameters(
      name = "testclient",
      sourceId = IdRange(0,1),
      requestFifo = true,
      visibility = Seq(AddressSet(0x0, 0xfff)))))))

  lazy val module = new LazyModuleImp(this) {
    val (out, edge) = TLClient.out(0)
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
      response := out.d.deq().data
    }

    // Hack to fix missing last instruction, fix later
    out.a.valid := addr < 0x20.U || (addr === 0x20.U && alt)
  }
}

class VerifTLClientPattern(txns: Seq[Pattern])(implicit p: Parameters) extends LazyModule  {
  val device = new SimpleDevice("VerifTLClientPattern", Seq("veriftldriver,veriftlmonitor,testclient"))

  // Filler for now
  val TLManager = TLRegisterNode(
    address = Seq(AddressSet(0x0, 0xfff)),
    device = device,
    beatBytes = 8,
    concurrency = 1)

  val patternp = LazyModule(new TLPatternPusher("patternpusher", txns))
  val TLClient = patternp.node

  lazy val module = new LazyModuleImp(this) {
    val testIO = IO(new Bundle {
      val run = Input(Bool())
      val done = Output(Bool())
    })

    RegNext(patternp.module.io.run) := testIO.run
    testIO.done := patternp.module.io.done
  }
}

class VerifTLClientFuzzer(implicit p: Parameters) extends LazyModule  {
  val device = new SimpleDevice("VerifTLClientFuzzer", Seq("veriftldriver,veriftlmonitor,testclient"))

  // Filler for now
  val TLManager = TLRegisterNode(
    address = Seq(AddressSet(0x0, 0xfff)),
    device = device,
    beatBytes = 8,
    concurrency = 1)

  val TLClient = TLFuzzer(30, inFlight=1)

  lazy val module = new LazyModuleImp(this) {}
}
