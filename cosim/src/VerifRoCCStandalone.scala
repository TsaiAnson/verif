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

class VerifRoCCStandaloneWrapper(dut: () => LazyRoCC, beatBytes: Int = 8, pAddrBits: Int = 32, addSinks: Int = 0, addSources: Int = 0)(implicit p: Parameters) extends LazyModule {
    lazy val ioOutNodes = new MutableList[BundleBridgeSink[TLBundle]]
    lazy val ioInNodes = new MutableList[BundleBridgeSource[TLBundle]]
    val dutInside = LazyModule(dut())

    for (i <- 0 until addSinks) {
      ioOutNodes += BundleBridgeSink[TLBundle]()
      ioOutNodes(i) :=
        TLToBundleBridge(VerifTestUtils.getVerifTLSlavePortParameters(beatBytes, pAddrBits, TransferSizes(1,64))) :=
        dutInside.tlNode
    }

    for (i <- 0 until addSources) {
      ioInNodes += BundleBridgeSource(() => TLBundle(VerifTestUtils.getVerifTLBundleParameters(beatBytes, pAddrBits, TransferSizes(1,64))))
      dutInside.tlNode :=
        BundleBridgeToTL(VerifTestUtils.getVerifTLMasterPortParameters()) :=
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
