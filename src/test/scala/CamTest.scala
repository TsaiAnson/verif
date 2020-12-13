package verif

import org.scalatest._
import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import designs.{CAMIO, ParameterizedCAMAssociative}
import chisel3.experimental.BundleLiterals._

class CamTest extends FlatSpec with ChiselScalatestTester {
	it should "cam test" in {
		test(new ParameterizedCAMAssociative(8,8,8))
			.withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
				implicit val randGen: VerifRandomGenerator = new ScalaVerifRandomGenerator
				val camInAgent = new GenericDriver[CAMIO](c.clock, c.io)
				val camOutAgent = new GenericMonitor[CAMIO](c.clock, c.io)
				val protoTx = CAMIO(8, 8)
				val inputTransactions = Seq(
					protoTx.Lit(_.en -> false.B, _.we -> true.B, _.keyRe -> 0.U, _.keyWr -> 10.U, _.dataWr -> 123.U, _.found -> false.B, _.dataRe -> 0.U),
					protoTx.Lit(_.en -> true.B, _.we -> false.B, _.keyRe -> 10.U, _.keyWr -> 0.U, _.dataWr -> 0.U, _.found -> false.B, _.dataRe -> 0.U)
				)

				camInAgent.push(inputTransactions)
				c.clock.step(inputTransactions.length + 1)
				val output = camOutAgent.getMonitoredTransactions.toArray[CAMIO]

				val model = new SWAssocCAM(8,8,8)
				val swoutput = inputTransactions.map(inpTx => model.process(inpTx)).toArray[CAMIO]

				assert(outputChecker.checkOutput(output.slice(1,output.size), {t : CAMIO => (t.found.litToBoolean, t.dataRe.litValue())},
				swoutput, {t : CAMIO => (t.found.litToBoolean, t.dataRe.litValue())}))
		}
	}
}