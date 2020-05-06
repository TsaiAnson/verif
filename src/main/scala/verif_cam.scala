package verif

import org.scalatest._

import chisel3._
import chiseltest._
import chisel3.util._

import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation

case class CAMIOInTr(en:Boolean, we:Boolean, keyRe: Int, keyWr: Int, dataWr: Int) extends Transaction

class CAMIOInTrNull() extends CAMIOInTr(false, false, 0, 0, 0)

case class CAMIOOutTr(found:Boolean, dataRe:Int) extends Transaction

class CamTest extends FlatSpec with ChiselScalatestTester {
	behavior of "Testers2 for CAM"

	it should "basic test to see if Transactions are working" in {
		test(new ParameterizedCAMAssociative(8,8,8)).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
			val camInAgent = new GenericDriver[CAMIOInTr](c)
			val camOutAgent = new GenericMonitor[CAMIOOutTr](c)
			val inputTransactions = Seq(
				CAMIOInTr(true,false,142,235,162),
				CAMIOInTr(true,false,118,75,168),
				CAMIOInTr(true,true,240,20,224),
				CAMIOInTr(false,false,230,69,71),
				new CAMIOInTrNull(),
				CAMIOInTr(false,false,44,133,131),
				CAMIOInTr(true,true,81,74,171),
				CAMIOInTr(true,true,125,145,165),
				CAMIOInTr(true,false,41,127,8),
				CAMIOInTr(true,true,129,105,197),
				CAMIOInTr(true,false,255,9,164),
				CAMIOInTr(false,true,78,44,251),
				CAMIOInTr(true,true,9,239,55),
				CAMIOInTr(false,true,75,34,212),
				CAMIOInTr(true,true,233,222,24),
				CAMIOInTr(true,false,209,40,141),
				CAMIOInTr(true,false,17,143,137),
				CAMIOInTr(true,true,217,141,6),
				CAMIOInTr(false,false,238,141,184),
				CAMIOInTr(false,true,219,254,152),
				CAMIOInTr(false,false,159,18,39),
				CAMIOInTr(false,false,185,50,210),
				CAMIOInTr(true,false,139,235,134),
				CAMIOInTr(false,true,145,130,101),
				CAMIOInTr(false,false,39,164,190),
				CAMIOInTr(true,true,193,224,25),
				CAMIOInTr(false,false,246,63,135),
				CAMIOInTr(true,false,212,189,150),
				CAMIOInTr(false,true,42,7,209),
				CAMIOInTr(false,true,231,223,95),
				CAMIOInTr(false,true,172,175,48),
				CAMIOInTr(false,true,82,242,182),
				CAMIOInTr(true,false,130,234,199),
				new CAMIOInTrNull(),
				CAMIOInTr(true,true,186,238,174),
				new CAMIOInTrNull(),
				CAMIOInTr(true,false,29,143,241),
				CAMIOInTr(false,false,72,116,21),
				CAMIOInTr(true,true,219,20,37),
				CAMIOInTr(false,true,248,142,220),
				CAMIOInTr(true,false,217,73,25),
				CAMIOInTr(true,true,165,161,51),
				CAMIOInTr(true,true,191,181,0),
				CAMIOInTr(true,false,28,250,119),
				CAMIOInTr(true,true,170,246,150),
				CAMIOInTr(false,false,200,241,189),
				CAMIOInTr(true,false,21,250,36),
				CAMIOInTr(false,true,5,169,192),
				CAMIOInTr(false,false,222,248,249),
				CAMIOInTr(true,true,229,11,47)
			)
			camInAgent.push(inputTransactions)
			c.clock.step(inputTransactions.length + 1)
			val output = camOutAgent.getMonitoredTransactions().toArray[CAMIOOutTr]

			val model = new SWAssocCAM(8,8,8)
			val swoutput = inputTransactions.map(inpTx => model.process(inpTx)).toArray[CAMIOOutTr]

			if (output.slice(1,output.size).deep == swoutput.deep) {
				println("***** PASSED *****")
			} else {
				println("***** FAILED *****")
				// Will need a better way of printing differences
				val outputTail = output.slice(1,output.size)
				println("========DUT========")
				for (t <- outputTail) {
					println(t.found, t.dataRe)
					println("========")
				}
				println("========GOLDEN MODEL========")
				for (t <- swoutput) {
					println(t.found, t.dataRe)
					println("========")
				}
			}
		}
	}
}

// object Test {
// 	def main(args: Array[String]): Unit = {
//     val moduleInst = new TestModule
//     val inputAgent = new GenericAgent[CAMIOInTr](moduleInst)
//     val inputTransactions = Seq(
//     	CAMIOInTr("0", "1", "2", "3", "4"),
//     	CAMIOInTr("4", "2", "3", "1", "0"),
//     	CAMIOInTr("0", "0", "0", "0", "0"),
//     	CAMIOInTr("1", "1", "1", "1", "1"),
//     	CAMIOInTr("2", "2", "2", "2", "2"),
//     	CAMIOInTr("3", "3", "3", "3", "3"),
//     	CAMIOInTr("4", "4", "4", "4", "4")
//     )
//     inputAgent.push(inputTransactions)
//   }
// }