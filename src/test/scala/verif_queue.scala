package verif

import org.scalatest._

import chisel3._
import chiseltest._
import chisel3.util._

import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation

// Process Enqueue first (artificial ordering by driver, need to fix later)
// The need of the data dequeued is due to the given Decoupled Driver. Fix later.
case class QueueIOInTr (readyDeq: Boolean, dataDeq: Int, validEnq: Boolean, dataEnq: Int)

class QueueIOInTrNull extends QueueIOInTr(false, 0,false, 0)

case class QueueIOOutTr (dataDeq: Int)

class QueueIOOutTrNull extends QueueIOOutTr(0)

class QueueModule[T <: Data](ioType: T, entries: Int) extends MultiIOModule {
  val in = IO(Flipped(Decoupled(ioType)))
  val out = IO(Decoupled(ioType))
  out <> Queue(in, entries)
}

class QueueTest extends FlatSpec with ChiselScalatestTester {
  behavior of "Decoupled Testers2 for Queue"

  it should "basic test to see if decoupled driver/monitor are working" in {
    test(new QueueModule(UInt(8.W), 5)).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
      c.in.initSource().setSourceClock(c.clock)
      c.out.initSink().setSinkClock(c.clock)

      val qInAgent = new DecoupledDriver[QueueIOInTr](c)
      val qOutAgent = new DecoupledMonitor[QueueIOOutTr](c)
      val inputTransactions = Seq(
        QueueIOInTr(false, 255, true, 3),
        QueueIOInTr(false, 255, true, 125),
        QueueIOInTr(true, 3, false, 255),
        QueueIOInTr(false, 255, true, 9),
        QueueIOInTr(true, 125, false, 255),
        QueueIOInTr(true, 9, true, 56),
        QueueIOInTr(true, 56, false, 255)
      )

      // Currently hardcoded for the Queue, will create a generic decoupled driver/
      // monitor that handles multiple inputs/outputs
      qInAgent.push(inputTransactions)

      val output = qOutAgent.getMonitoredTransactions.toArray[QueueIOOutTr]

      val model = new SWIntQueue(5);
      val swoutput = inputTransactions.map(inpTx => model.process(inpTx)).filter(!_.isInstanceOf[QueueIOOutTrNull])
        .toArray[QueueIOOutTr]

      if (output.deep == swoutput.deep) {
        println("***** PASSED *****")
        val outputsize = output.length
        println(s"All $outputsize transactions were matched.")
      } else {
        println("***** FAILED *****")
        // Will need a better way of printing differences
        println("========DUT========")
        for (t <- output) {
          println(t)
        }
        println("========GOLDEN MODEL========")
        for (t <- swoutput) {
          println(t)
        }
      }
    }
  }
}