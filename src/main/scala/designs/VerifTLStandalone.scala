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

  val TLClient: TLNode
  val TLManager: TLNode

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
    var addr = 0.U(6.W)
    var alt = 0.U(10.W)
    var response: TLBundleD = out.d.deq()

    // Read and write back values
    // Hardcoded example
    // Read data
//    var request = edge.Get(0.U, 0.U, 3.U)._2
//    out.a.enq(request)
//    var response = out.d.deq()
//    // Write data
//    request = edge.Put(0.U, 0x20.U, 3.U, response.data)._2
//    out.a.enq(request)
//    response = out.d.deq()
//    // Repeat 3 more times
//    request = edge.Get(0.U, 0x08.U, 3.U)._2
//    out.a.enq(request)
//    response = out.d.deq()
//    request = edge.Put(0.U, 0x28.U, 3.U, response.data)._2
//    out.a.enq(request)
//    response = out.d.deq()
//
//    request = edge.Get(0.U, 0x10.U, 3.U)._2
//    out.a.enq(request)
//    response = out.d.deq()
//    request = edge.Put(0.U, 0x30.U, 3.U, response.data)._2
//    out.a.enq(request)
//    response = out.d.deq()
//
//    request = edge.Get(0.U, 0x18.U, 3.U)._2
//    out.a.enq(request)
//    response = out.d.deq()
//    request = edge.Put(0.U, 0x38.U, 3.U, response.data)._2
//    out.a.enq(request)
//    response = out.d.deq()

    when (out.a.ready) {
      if ((alt % 2.U).litValue().toInt == 1) {
        response = out.d.deq()
        out.a.enq(edge.Put(0.U, addr + 0x20.U, 3.U, response.data)._2)
        addr = addr + 0x08.U
        alt = alt + 1.U
      } else {
        response = out.d.deq()
        out.a.enq(edge.Get(0.U, addr, 3.U)._2)
        alt = alt + 1.U
      }
    }
  }
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

