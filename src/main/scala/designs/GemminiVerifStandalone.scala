package designs

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile.{TileVisibilityNodeKey, OpcodeSet}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink.TLRegisterNode
import verif.VerifTLBase
import gemmini._

class GemminiVerifStandaloneBlock(implicit p: Parameters) extends LazyModule with VerifTLBase {
  //val ioInNode = BundleBridgeSource(() => TLBundle(verifTLUBundleParams))
  val ioOutNode = BundleBridgeSink[TLBundle]()

  val gemmini = LazyModule(new Gemmini(OpcodeSet.custom3, GemminiConfigs.defaultConfig))

  //gemmini.tlNode :=
  //  BundleBridgeToTL(TLClientPortParameters(Seq(TLClientParameters("bundleBridgeToTL")))) :=
  //  ioInNode

  ioOutNode :=
    TLToBundleBridge(TLManagerPortParameters(Seq(TLManagerParameters(address = Seq(AddressSet(0x0, 0xfff)),
      supportsGet = TransferSizes(1, 16), supportsPutFull = TransferSizes(1,16), supportsPutPartial = TransferSizes(1,16))), 16)) :=
    gemmini.tlNode

  val out = InModuleBody { ioOutNode.makeIO() }

  lazy val module = new LazyModuleImp(this) {

  }
}
