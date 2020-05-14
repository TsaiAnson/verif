package verif

import org.scalatest._

import chisel3._
import chiseltest._
import chisel3.util._

import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation

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

      val qOutAgent = new DecoupledMonitor[UInt] (c.out, c)
      val inputTransactionsC = Seq(3.U, 125.U, 9.U, 56.U)
      val inputTransactions = Seq(3, 125, 9, 56)

      // Currently hardcoded for the Queue, will create a generic decoupled driver/
      // monitor that handles multiple inputs/outputs
      fork {
        c.in.enqueueSeq(inputTransactionsC)
      }

      c.out.expectInvalid()
      c.clock.step(1)  // wait for first element to enqueue
      c.out.expectDequeueNow(3.U)
      c.out.expectDequeueNow(125.U)
      c.out.expectDequeueNow(9.U)
      c.out.expectDequeueNow(56.U)
      c.out.expectInvalid()

      val output = qOutAgent.getMonitoredTransactions.toArray[UInt].map(chiseltype => chiseltype.litValue().toInt)

      // Again, will need to make a wrapper of how to handle transactions
      val model = new SWIntQueue(5);
      model.enqueueSeq(inputTransactions)
      val swoutput = model.dequeueAll.toArray[Int]

      if (output.deep == swoutput.deep) {
        println("***** PASSED *****")
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