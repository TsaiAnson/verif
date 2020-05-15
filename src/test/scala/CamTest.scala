package verif

import org.scalatest._

import chisel3._
import chiseltest._
import chisel3.util._

import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation

class CamTest extends FlatSpec with ChiselScalatestTester {

	it should "cam test" in {
		test(new ParameterizedCAMAssociative(8,8,8)).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
			val camInAgent = new GenericDriver[CAMIOInTr](c.clock, c.in)
			val camOutAgent = new GenericMonitor[CAMIOOutTr](c.clock, c.out, {(intf: CAMIOOutTr) => CAMIOOutTr(intf.found, intf.dataRe)})
			val inputTransactions = Seq(
				CAMIOInTr(false.B, true.B, 0.U, 10.U, 123.U),
				CAMIOInTr(true.B, false.B, 10.U, 0.U, 0.U)
			)
			camInAgent.push(inputTransactions)
			c.clock.step(inputTransactions.length + 1)
			val output = camOutAgent.getMonitoredTransactions
			for (t <- output) {
				print(t.found.litValue())
				println(t.dataRe.litValue())
			}
//			val model = new SWAssocCAM(8,8,8)
//			val swoutput = inputTransactions.map(inpTx => model.process(inpTx)).toArray[CAMIOOutTr]
//
//			if (output.slice(1,output.size).deep == swoutput.deep) {
//				println("***** PASSED *****")
//			} else {
//				println("***** FAILED *****")
//				// Will need a better way of printing differences
//				val outputTail = output.slice(1,output.size)
//				println("========DUT========")
//				for (t <- outputTail) {
//					println(t.found, t.dataRe)
//					println("========")
//				}
//				println("========GOLDEN MODEL========")
//				for (t <- swoutput) {
//					println(t.found, t.dataRe)
//					println("========")
//				}
//			}
		}
	}
}