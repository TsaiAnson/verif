package verif

import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest._
import chisel3.util._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}

class QueueTest extends AnyFlatSpec with ChiselScalatestTester {

  it should "Queue Test" in {
    test(new Queue(UInt(8.W), 8)).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>

      val qInAgent = new DecoupledDriver[UInt](c.clock, c.io.enq)

      val waitCycles = 2
      val qOutAgent = new DecoupledMonitor[UInt](c.clock, c.io.deq)
      qOutAgent.setConfig("waitCycles", waitCycles)

      // Must ensure that there are enough cycles for the whole test
      val simCycles = 80
      val inputTransactions = Seq(
        DecoupledTX(165.U,0.U,1.U),
        DecoupledTX(122.U,1.U,1.U),
        DecoupledTX(227.U,2.U,3.U),
        DecoupledTX(227.U,1.U,1.U),
        DecoupledTX(239.U,1.U,0.U),
        DecoupledTX(108.U,2.U,1.U),
        DecoupledTX(226.U,2.U,0.U),
        DecoupledTX(27.U,1.U,0.U),
        DecoupledTX(81.U,1.U,1.U),
        DecoupledTX(127.U,0.U,2.U),
        DecoupledTX(199.U,0.U,1.U),
        DecoupledTX(161.U,1.U,1.U),
        DecoupledTX(21.U,2.U,3.U),
        DecoupledTX(161.U,3.U,2.U),
        DecoupledTX(59.U,0.U,0.U),
        DecoupledTX(89.U,0.U,0.U),
        DecoupledTX(191.U,1.U,0.U),
        DecoupledTX(107.U,2.U,0.U),
        DecoupledTX(251.U,1.U,2.U),
        DecoupledTX(210.U,0.U,1.U)
      )

      qInAgent.push(inputTransactions)
      c.clock.step(simCycles)

      val output = qOutAgent.getMonitoredTransactions.toArray[DecoupledTX[UInt]]

      val model = new SWIntQueue(8)
      val swoutput = model.process(inputTransactions, simCycles, waitCycles).toArray[DecoupledTX[UInt]]

      // Cycle offset between software and DUT
      val cycleOffset = 2

      assert(outputChecker.checkOutput(output, {t : DecoupledTX[UInt] => (t.data.litValue(), t.cycleStamp - cycleOffset)},
        swoutput, {t : DecoupledTX[UInt] => (t.data.litValue(), t.cycleStamp)}))
    }
  }
}