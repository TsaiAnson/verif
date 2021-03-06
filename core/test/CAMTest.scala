package verif

import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}
import designs.{CAMIO, ParameterizedCAMAssociative}
import chisel3.experimental.BundleLiterals._

class CAMTest extends AnyFlatSpec with ChiselScalatestTester {
  it should "cam test" in {
    test(new ParameterizedCAMAssociative(8,8,8)) { c =>
      val protoTx = CAMIO(8, 8)
      val inputTransactions = Seq(
        protoTx.Lit(_.en -> false.B, _.we -> true.B, _.keyRe -> 0.U, _.keyWr -> 10.U, _.dataWr -> 123.U, _.found -> false.B, _.dataRe -> 0.U),
        protoTx.Lit(_.en -> true.B, _.we -> false.B, _.keyRe -> 10.U, _.keyWr -> 0.U, _.dataWr -> 0.U, _.found -> false.B, _.dataRe -> 0.U)
      )

      val output = inputTransactions.foldLeft(Seq.empty[CAMIO]){ case (seenTxns, toPush) =>
        c.io.pokePartial(toPush)
        val n = seenTxns :+ c.io.peek()
        c.clock.step()
        n
      }

      val model = new SWAssocCAM(8,8,8)
      val swoutput = inputTransactions.map(inpTx => model.process(inpTx))

      output.zip(swoutput).foreach {
        case (dut_out, sw_out) =>
          assert(dut_out.found.litToBoolean == sw_out.found.litToBoolean)
          assert(dut_out.dataRe.litValue() == sw_out.dataRe.litValue())
      }
    }
  }
}