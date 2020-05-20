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

      val waitCycles = 2
      val qOutAgent = new DecoupledMonitor[UInt](c.clock, c.io.deq)
      qOutAgent.setWaitCycles(waitCycles)

      val simCycles = 80
      val inputTransactions = Seq(
        DecoupledTX(165.U,0,1),
        DecoupledTX(122.U,1,1),
        DecoupledTX(227.U,2,3),
        DecoupledTX(227.U,1,1),
        DecoupledTX(239.U,1,0),
        DecoupledTX(108.U,2,1),
        DecoupledTX(226.U,2,0),
        DecoupledTX(27.U,1,0),
        DecoupledTX(81.U,1,1),
        DecoupledTX(127.U,0,2),
        DecoupledTX(199.U,0,1),
        DecoupledTX(161.U,1,1),
        DecoupledTX(21.U,2,3),
        DecoupledTX(161.U,3,2),
        DecoupledTX(59.U,0,0),
        DecoupledTX(89.U,0,0),
        DecoupledTX(191.U,1,0),
        DecoupledTX(107.U,2,0),
        DecoupledTX(251.U,1,2),
        DecoupledTX(210.U,0,1)
      )

      qInAgent.push(inputTransactions)
      c.clock.step(simCycles)

      val output = qOutAgent.getMonitoredTransactions.toArray[DecoupledTX[UInt]]

      val model = new SWIntQueue(8)
      val swoutput = model.process(inputTransactions, simCycles, waitCycles).toArray[DecoupledTX[UInt]]

      // Cycle offset between software and DUT
      val cycleOffset = 2

      if (output.map(t => (t.data.litValue(), t.cycleStamp - cycleOffset)).sameElements(
        swoutput.map(t => (t.data.litValue(), t.cycleStamp)))) {
        println("***** PASSED *****")
        val outputsize = output.length
        println(s"All $outputsize transactions were matched.")
      } else {
        println("***** FAILED *****")
        // Will need a better way of printing differences
        println("========DUT========")
        for (t <- output) {
          println(t.data.litValue(), t.cycleStamp)
        }
        println("========GOLDEN MODEL========")
        for (t <- swoutput) {
          println(t.data.litValue(), t.cycleStamp)
        }
      }
    }
  }
}
