package verif

import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest._
import chisel3.util._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}

class QueueTest extends AnyFlatSpec with ChiselScalatestTester {
  def testFn(c: Clock, enq: DecoupledIO[UInt], deq: DecoupledIO[UInt], gen: UInt): Unit = {
    val enqDriver = new DecoupledDriverMaster(c, enq)

    val waitCycles = 2
    val deqDriver = new DecoupledDriverSlave(c, deq, waitCycles)
    val monitor = new DecoupledMonitor(c, deq)

    val txProto = new DecoupledTX(gen)
    val inputTransactions = Seq(
      txProto.tx(165.U, 0, 1),
      txProto.tx(122.U,1,1),
      txProto.tx(227.U,2,3),
      txProto.tx(227.U,1,1),
      txProto.tx(239.U,1,0),
      txProto.tx(108.U,2,1),
      txProto.tx(226.U,2,0),
      txProto.tx(27.U,1,0),
      txProto.tx(81.U,1,1),
      txProto.tx(127.U,0,2),
      txProto.tx(199.U,0,1),
      txProto.tx(161.U,1,1),
      txProto.tx(21.U,2,3),
      txProto.tx(161.U,3,2),
      txProto.tx(59.U,0,0),
      txProto.tx(89.U,0,0),
      txProto.tx(191.U,1,0),
      txProto.tx(107.U,2,0),
      txProto.tx(251.U,1,2),
      txProto.tx(210.U,0,1)
    )
    // Must ensure that there are enough cycles for the whole test
    val simCycles = inputTransactions.length * (
      inputTransactions.map(_.waitCycles.litValue()).max +
        inputTransactions.map(_.postSendCycles.litValue()).max
      )

    enqDriver.push(inputTransactions)
    c.step(simCycles.toInt)

    val output = monitor.monitoredTransactions

    val model = new SWQueue(8, gen)
    val swoutput = model.process(inputTransactions, simCycles.toInt, waitCycles)

    // Cycle offset between software and DUT
    val cycleOffset = 2

    output.zip(swoutput).foreach {
      case (dut_out, sw_out) =>
        assert(dut_out.data.litValue() == sw_out.data.litValue())
        assert(dut_out.cycleStamp.litValue() - cycleOffset == sw_out.cycleStamp.litValue())
    }
  }

  val gen: UInt = UInt(8.W)
  it should "test the chisel3 Queue" in {
    test(new Queue(gen, 8)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      testFn(c.clock, c.io.enq, c.io.deq, gen)
    }
  }

  it should "test the rocketchip ShiftQueue" in {
    test(new ShiftQueue(gen, 8)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      testFn(c.clock, c.io.enq, c.io.deq, gen)
    }
  }
}
