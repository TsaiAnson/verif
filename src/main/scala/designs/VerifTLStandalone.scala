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
//  val ioOutNode = BundleBridgeSink[TLBundle]()
  val TLNode: TLNode

//  ioOutNode :=
//    TLToBundleBridge(TLManagerPortParameters(Seq(TLManagerParameters(address = Seq(AddressSet(0x2000, 0xfff)))), beatBytes = 8)) :=
    TLNode :=
    BundleBridgeToTL(TLClientPortParameters(Seq(TLClientParameters("bundleBridgeToTL")))) :=
    ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
//  val out = InModuleBody { ioOutNode.makeIO() }
}

//trait VerifTLBasicBlock extends VerifTLStandaloneBlock {
//  def csrAddress = AddressSet(0x0, 0xff)
//  def beatBytes = 8
//  def memdevname = "veriftlbasicblock"
//  def devcompat = Seq("verif")
//  val memdevice = new SimpleDevice(memdevname, devcompat) {
//    override def describe(resources: ResourceBindings): Description = {
//      val Description(name, mapping) = super.describe(resources)
//      Description(name, mapping)
//    }
//  }
//  override val mem = Some(TLRegisterNode(address = Seq(csrAddress), device = memdevice, beatBytes = beatBytes))
//}

//class VerifTLPassthrough(implicit p: Parameters) extends LazyModule with VerifTLBasicBlock  {
class VerifTLPassthrough(implicit p: Parameters) extends LazyModule  {
  val device = new SimpleDevice("veriftlpassthrough", Seq("veriftldriver,veriftlmonitor")) // Not sure about compatibility list
//  val TLNode = TLIdentityNode()
//
//  TLNode := node

  val TLNode = TLRegisterNode(
    address = Seq(AddressSet(0x2000, 0xfff)),
    device = device,
    beatBytes = 8)

  lazy val module = new LazyModuleImp(this) {
    val bigReg = RegInit(10.U(64.W))
    val mediumReg = RegInit(11.U(32.W))
    val smallReg = RegInit(12.U(16.W))

    val tinyReg0 = RegInit(13.U(4.W))
    val tinyReg1 = RegInit(14.U(4.W))

    TLNode.regmap(
      0x00 -> Seq(RegField(64, bigReg)),
      0x08 -> Seq(RegField(32, mediumReg)),
      0x0C -> Seq(RegField(16, smallReg)),
      0x0E -> Seq(RegField(4, tinyReg0),
                  RegField(4, tinyReg1))
    )

    val (in, _) = TLNode.in.unzip
    val (out, _) = TLNode.out.unzip
  }
}

