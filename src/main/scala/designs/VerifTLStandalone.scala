package designs

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink.TLRegisterNode

trait VerifTLStandaloneBlock extends LazyModule {
  def standaloneParams = TLBundleParameters(addressBits = 64, dataBits = 64, sourceBits = 1,
    sinkBits = 1, sizeBits = 6,
    echoFields = Seq(), requestFields = Seq(), responseFields = Seq(),
    hasBCE = false)

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

  val ioInNode = BundleBridgeSource(() => TLBundle(standaloneParams))
  val ioOutNode = BundleBridgeSink[TLBundle]()

  val TLClient: TLOutwardNode
  val TLManager: TLInwardNode

   ioOutNode :=
     TLToBundleBridge(TLManagerPortParameters(Seq(TLManagerParameters(address = Seq(AddressSet(0x0, 0xfff)),
       supportsGet = TransferSizes(1, 8), supportsPutFull = TransferSizes(1,8))), beatBytes = 8)) :=
     TLClient

  TLManager :=
    BundleBridgeToTL(TLClientPortParameters(Seq(TLClientParameters("bundleBridgeToTL")))) :=
    ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }
}

class VerifTLPassthroughManager(implicit p: Parameters) extends LazyModule  {
  val device = new SimpleDevice("veriftlpassthroughmanager", Seq("veriftldriver,veriftlmonitor,testclient")) // Not sure about compatibility list

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

// TODO: FIX
class VerifTLPassthroughClient(implicit p: Parameters) extends LazyModule  {
  val device = new SimpleDevice("veriftlpassthroughclient", Seq("veriftldriver,veriftlmonitor,testclient"))

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
        val addr = RegInit(UInt(6.W), 0.U)
        val alt = RegInit(Bool(), false.B)
        val response = out.d.deq()

        when (out.a.ready) {
          when (alt === true.B) {
            response := out.d.deq()
            out.a.enq(edge.Put(0.U, addr + 0x20.U, 3.U, 1.U)._2)
            addr := addr + 0x08.U
            alt := !alt
          } otherwise {
            response := out.d.deq()
            out.a.enq(edge.Get(0.U, addr, 3.U)._2)
            alt := !alt
          }
        }
  }
}

class VerifTLPassthroughClientPattern(txns: Seq[Pattern])(implicit p: Parameters) extends LazyModule  {
  val device = new SimpleDevice("veriftlpassthroughclient", Seq("veriftldriver,veriftlmonitor,testclient"))

  // Filler for now
  val TLManager = TLRegisterNode(
    address = Seq(AddressSet(0x0, 0xfff)),
    device = device,
    beatBytes = 8,
    concurrency = 1)

  val TLClient = TLPatternPusher("testclient", txns)

  lazy val module = new LazyModuleImp(this) {}
}

class VerifTLPassthroughClientFuzzer(implicit p: Parameters) extends LazyModule  {
  val device = new SimpleDevice("veriftlpassthroughclient", Seq("veriftldriver,veriftlmonitor,testclient"))

  // Filler for now
  val TLManager = TLRegisterNode(
    address = Seq(AddressSet(0x0, 0xfff)),
    device = device,
    beatBytes = 8,
    concurrency = 1)

  val TLClient = TLFuzzer(30, inFlight=1)

  lazy val module = new LazyModuleImp(this) {}
}

//class RegMaster(implicit p: Parameters) extends LazyModule {
//  val device = new SimpleDevice("veriftlpassthrough", Seq("veriftldriver,veriftlmonitor,testclient"))
//
//  val RegNode = TLRegisterNode(
//    address = Seq(AddressSet(0x0, 0xfff)),
//    device = device,
//    beatBytes = 8,
//    concurrency = 1)
//
//  lazy val module = new LazyModuleImp(this) {
//    val bigReg = RegInit(10.U(64.W))
//    val mediumReg = RegInit(11.U(32.W))
//    val smallReg = RegInit(12.U(16.W))
//
//    val tinyReg0 = RegInit(13.U(4.W))
//    val tinyReg1 = RegInit(14.U(4.W))
//
//    // Will try to implement hardware FIFO (using Queue) for later examples
//    RegNode.regmap(
//      0x00 -> Seq(RegField(64, bigReg)),
//      0x08 -> Seq(RegField(32, mediumReg)),
//      0x0C -> Seq(RegField(16, smallReg)),
//      0x0E -> Seq(RegField(4, tinyReg0),
//        RegField(4, tinyReg1))
//    )
//
//    val (reg, regE) = RegNode.in(0)
//  }
//}
//
//class Client1(implicit p: Parameters) extends LazyModule {
//  val client = TLClientNode(Seq(TLClientPortParameters(Seq(TLClientParameters(
//    name = "testclient",
//    sourceId = IdRange(0,1),
//    requestFifo = true,
//    visibility = Seq(AddressSet(0x1000, 0xfff)))))))
//
//  lazy val module = new LazyModuleImp(this) {
//    val (cli, cliE) = client.out(0)
//  }
//}
//
//class MasterClient(implicit p: Parameters) extends LazyModule {
//  val master = LazyModule(new RegMaster)
//  val client = LazyModule(new Client1)
//
//  master.RegNode := client.client
//
//  lazy val module = new LazyModuleImp(this) {
//    val reg = master.module.reg
//    val regE = master.module.regE
//    val cli = client.module.cli
//    val cliE = client.module.cliE
//  }
//}

