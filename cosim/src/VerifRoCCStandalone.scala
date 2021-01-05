package designs

import chisel3._
import chisel3.experimental.{IO}
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink.TLRegisterNode
import scala.collection.mutable.MutableList
import verif._

class VerifRoCCStandaloneWrapper(dut: () => LazyRoCC, beatBytes: Int = 8, addSinks: Int = 0, addSources: Int = 0)(implicit p: Parameters) extends LazyModule {
  def verifTLUBundleParams: TLBundleParameters = TLBundleParameters(addressBits = 64, dataBits = 64, sourceBits = 1,
    sinkBits = 1, sizeBits = 6,
    echoFields = Seq(), requestFields = Seq(), responseFields = Seq(),
    hasBCE = false)

    lazy val ioOutNodes = new MutableList[BundleBridgeSink[TLBundle]]
    lazy val ioInNodes = new MutableList[BundleBridgeSource[TLBundle]]
    val dutInside = LazyModule(dut())

    for (i <- 0 until addSinks) {
      ioOutNodes += BundleBridgeSink[TLBundle]()
      ioOutNodes(i) :=
        TLToBundleBridge(TLManagerPortParameters(Seq(TLManagerParameters(address = Seq(AddressSet(0x0, 0xfff)),
          supportsGet = TransferSizes(1, 64), supportsPutFull = TransferSizes(1,64), supportsPutPartial = TransferSizes(1,64))), beatBytes)) :=
        dutInside.tlNode
    }

    for (i <- 0 until addSources) {
      ioInNodes += BundleBridgeSource(() => TLBundle(verifTLUBundleParams))
      dutInside.tlNode :=
        BundleBridgeToTL(TLClientPortParameters(Seq(TLClientParameters("bundleBridgeToTL")))) :=
        ioInNodes(i)
    }

  lazy val module = new VerifRoCCStandaloneWrapperModule(this)
}

class VerifRoCCStandaloneWrapperModule(outer: VerifRoCCStandaloneWrapper) extends LazyModuleImp(outer) {
  import outer.dutInside
  import outer.ioInNodes
  import outer.ioOutNodes

  val io = IO(new RoCCIO(dutInside.nPTWPorts))
  io <> dutInside.module.io

  val tlOut = ioOutNodes.map{ (outNode) => outNode.makeIO()}
  val tlIn = ioInNodes.map{ (inNode) => inNode.makeIO()}
}
