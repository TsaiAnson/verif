package verif

import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import chisel3.util.{Queue, QueueIO}
import chisel3._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
import verif.StimulusGenerator.OneShotState


class TestComponentTest extends AnyFlatSpec with ChiselScalatestTester {
  it should "run" in {
    val gen = UInt(8.W)
    val dut = () => new Queue(gen, 8, false, false)

    // VIPs
    val master = new DecoupledMaster(gen)
    val slave = new DecoupledSlave(gen, 10)
    type Transaction = (DecoupledMasterTX[UInt], DecoupledSlaveTX)
    type Emission = (Option[DecoupledMonTX[UInt]], Option[DecoupledMonTX[UInt]])

    val tester: TestComponent[QueueIO[UInt], Transaction, (MasterState[UInt], SlaveState), Emission] =
      TestComponent.combine(master, (io: QueueIO[UInt]) => io.enq, slave, (io: QueueIO[UInt]) => io.deq)

    // Stimulus
    val masterTxProto = DecoupledMasterTX(gen)
    val stim: StimulusGenerator[Transaction, Emission, OneShotState] =
      StimulusGenerator.oneShot(Seq.tabulate(10){ i => (masterTxProto.tx(i.U, 0), DecoupledSlaveTX(10-i))})
    val obj = Objection.waitForN(10, (e: Emission) => e._2.isDefined)

    test(dut()).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val output = TestComponent.runSim(c.clock, c.io, tester, stim, obj)
      val masterTx = output.flatMap(_._1)
      println(masterTx)
      val slaveTx = output.flatMap(_._2)
      println(slaveTx)
    }
  }
}
