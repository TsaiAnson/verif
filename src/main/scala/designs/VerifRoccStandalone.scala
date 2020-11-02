package designs

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink.TLRegisterNode
import verif.VerifTLBase

class VerifRoCCStandaloneWrapper(dut: LazyRoCC, beatBytes: Int = 8)(implicit p: Parameters) extends LazyModule with VerifTLBase {
  val ioOutNode = BundleBridgeSink[TLBundle]()

  children = dut::children

  ioOutNode :=
    TLToBundleBridge(TLManagerPortParameters(Seq(TLManagerParameters(address = Seq(AddressSet(0x0, 0xfff)),
      supportsGet = TransferSizes(1, 64), supportsPutFull = TransferSizes(1,64), supportsPutPartial = TransferSizes(1,64))), beatBytes)) :=
    dut.tlNode

  val out = InModuleBody {ioOutNode.makeIO()}

  lazy val module = new LazyModuleImp(this) {}
}