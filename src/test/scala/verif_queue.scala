package verif

import org.scalatest._
import chisel3._
import chiseltest._
import chisel3.util._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, VerilatorBackendAnnotation, WriteVcdAnnotation}

// Process Enqueue first (artificial ordering by driver, need to fix later)
// The need of the data dequeued is due to the given Decoupled Driver. Fix later.

class BundleA extends Bundle {
  val x = UInt(8.W)
  val y = SInt(8.W)
}
/*
interface BundleA begin
  reg[7:0] x;
  signed reg[7:0] y;
end

reg[7:0] x;
signed reg[7:0] y;

Queue#(8) q(
  .*
);

driver = new DecoupledDriver(q.enq)
driver.push(DecoupledTX(new BundleA(x=10.U, y=-3.S))

 */

//val q = Queue[BundleA](8)
//val driver = DecoupledDriver(q.enq)
//driver.push(DecoupledTX(new BundleA(x=10.U, y=-3.S))
case class DecoupledTX[T <: Data](data: T, waitCycles: Int = 0, postSendCycles: Int = 0)
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
  //behavior of "Decoupled Testers2 for Queue"

  it should "queue test" in {
    test(new Queue(UInt(8.W), 8)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val qInAgent = new DecoupledDriver[UInt](c.clock, c.io.enq)
      val qOutAgent = new DecoupledMonitor[UInt](c.clock, c.io.deq)

      val inputTransactions = Seq(
        DecoupledTX(10.U),
        DecoupledTX(1.U)
      )

      // Currently hardcoded for the Queue, will create a generic decoupled driver/
      // monitor that handles multiple inputs/outputs
      qInAgent.push(inputTransactions)
        c.clock.step(10)
      val output = qOutAgent.txns
      /*
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
       */
      println("Received data:")
      for (t <- output) {
        println(t.data.litValue())
      }
    }
  }
}