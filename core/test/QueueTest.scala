package verif

import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest._
import chisel3.util._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}

class QueueTest extends AnyFlatSpec with ChiselScalatestTester {
  it should "test the chisel3 Queue" in {
    val gen = UInt(8.W)
    test(new Queue(gen, 8)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val qInAgent = new DecoupledDriver[UInt](c.clock, c.io.enq)

      val waitCycles = 2
      val qOutAgent = new DecoupledMonitor[UInt](c.clock, c.io.deq, waitCycles)

      val txProto = DecoupledTX(gen)
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

      qInAgent.push(inputTransactions)
      c.clock.step(simCycles.toInt)

      val output = qOutAgent.getMonitoredTransactions

      val model = new SWQueue(8, gen)
      val swoutput = model.process(inputTransactions, simCycles.toInt, waitCycles)

      // Cycle offset between software and DUT
      val cycleOffset = 2
      //output.foreach(tx => println(tx.data, tx.cycleStamp))
      //swoutput.foreach(tx => println(tx.data, tx.cycleStamp))

      output.zip(swoutput).foreach {
        case (dut_out, sw_out) =>
          assert(dut_out.data.litValue() == sw_out.data.litValue())
          assert(dut_out.cycleStamp.litValue() - cycleOffset == sw_out.cycleStamp.litValue())
      }
    }
  }
}
