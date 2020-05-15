package verif

import org.scalatest._
import chisel3._
import chiseltest._
import chisel3.util._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, VerilatorBackendAnnotation, WriteVcdAnnotation}

//class BundleA extends Bundle {
//  val x = UInt(8.W)
//  val y = SInt(8.W)
//}

class QueueTest extends FlatSpec with ChiselScalatestTester {

  it should "Queue Test" in {
    test(new Queue(UInt(8.W), 8)).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      val qInAgent = new DecoupledDriver[UInt](c.clock, c.io.enq)
      val qOutAgent = new DecoupledMonitor[UInt](c.clock, c.io.deq)

      val inputTransactions = Seq(
        DecoupledTX(10.U),
        DecoupledTX(1.U),
        DecoupledTX(15.U)
      )

      qInAgent.push(inputTransactions)
      c.clock.step(inputTransactions.size + 1)

      val output = qOutAgent.getMonitoredTransactions.toArray[DecoupledTX[UInt]]

      val model = new SWIntQueue(8);
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