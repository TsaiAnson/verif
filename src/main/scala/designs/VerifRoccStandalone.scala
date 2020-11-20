package designs

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink.TLRegisterNode
import verif._

class VerifRoCCStandaloneWrapper(dut: () => LazyRoCC, beatBytes: Int = 8)(implicit p: Parameters) extends LazyModule {

  lazy val ioOutNode = BundleBridgeSink[TLBundle]()

  val dutInside = LazyModule(dut())

  ioOutNode :=
    TLToBundleBridge(TLManagerPortParameters(Seq(TLManagerParameters(address = Seq(AddressSet(0x0, 0xfff)),
      supportsGet = TransferSizes(1, 64), supportsPutFull = TransferSizes(1,64), supportsPutPartial = TransferSizes(1,64))), beatBytes)) :=
    dutInside.tlNode

  //val tlOut = InModuleBody { ioOutNode.makeIO() }

  lazy val module = new VerifRoCCStandaloneWrapperModule(this)
}

class VerifRoCCStandaloneWrapperModule(outer: VerifRoCCStandaloneWrapper) extends LazyModuleImp(outer) {
  import outer.dutInside
  import outer.ioOutNode

  val io = IO(new Bundle {
    val cmdIn = Flipped(Decoupled(new RoCCCommand))
  })

  val tlOut = ioOutNode.makeIO()

  val cmdQueue = Module(new RoCCCommandQueue(depth = 4))
  cmdQueue.io.in <> io.cmdIn
  dutInside.module.io.cmd <> cmdQueue.io.out

}