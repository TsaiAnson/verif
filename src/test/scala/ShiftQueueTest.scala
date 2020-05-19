package verif

import org.scalatest._
import chisel3._
import chiseltest._
import chisel3.util._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}
import freechips.rocketchip.util.ShiftQueue

class ShiftQueueTest extends FlatSpec with ChiselScalatestTester {

  it should "ShiftQueue Test" in {
    test(new ShiftQueue(UInt(8.W), 8)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val qInAgent = new DecoupledDriver[UInt](c.clock, c.io.enq)
      val qOutAgent = new DecoupledMonitor[UInt](c.clock, c.io.deq)

      val inputTransactions = Seq(
        DecoupledTX(10.U),
        DecoupledTX(1.U),
        DecoupledTX(253.U),
        DecoupledTX(0.U),
        DecoupledTX(64.U),
        DecoupledTX(47.U),
        DecoupledTX(23.U),
        DecoupledTX(78.U),
        DecoupledTX(173.U),
        DecoupledTX(221.U),
        DecoupledTX(85.U),
        DecoupledTX(34.U),
        DecoupledTX(94.U),
        DecoupledTX(33.U),
        DecoupledTX(198.U),
        DecoupledTX(102.U),
        DecoupledTX(31.U),
        DecoupledTX(57.U),
        DecoupledTX(134.U),
        DecoupledTX(96.U),
        DecoupledTX(43.U)
      )

      qInAgent.push(inputTransactions)
      c.clock.step(inputTransactions.size + 1)

      val output = qOutAgent.getMonitoredTransactions.toArray[DecoupledTX[UInt]]

      val model = new SWIntQueue(8)
      val swoutput = inputTransactions.map(inpTx => model.process(inpTx)).toArray[DecoupledTX[UInt]]

      if (output.map(t => t.data.litValue()).sameElements(swoutput.map(t => t.data.litValue()))) {
        println("***** PASSED *****")
        val outputsize = output.length
        println(s"All $outputsize transactions were matched.")
      } else {
        println("***** FAILED *****")
        // Will need a better way of printing differences
        println("========DUT========")
        for (t <- output) {
          println(t.data.litValue())
        }
        println("========GOLDEN MODEL========")
        for (t <- swoutput) {
          println(t.data.litValue())
        }
      }
    }
  }
}
