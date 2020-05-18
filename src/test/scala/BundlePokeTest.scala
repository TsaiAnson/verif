package verif

import org.scalatest._
import chisel3._
import chiseltest._
import chisel3.util._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}
import chisel3.experimental.BundleLiterals._

// Defining module here for simplicity
case class MultiDirBundleIO() extends Bundle {
  val en = Input(Bool())
  val input = Input(UInt(8.W))
  val output = Output(UInt(8.W))
}

class MultiDirBundleIOModule extends MultiIOModule {
  val io = IO(new MultiDirBundleIO)

  io.output := io.input
}

class MultiDirBundlePokeTest extends FlatSpec with ChiselScalatestTester {
  it should "multi dir bundle poke test" in {
    test(new MultiDirBundleIOModule).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>

      val protoTx = MultiDirBundleIO()
      val inputTransactions = Seq(
        protoTx.Lit(_.en -> false.B, _.input -> 0.U, _.output -> 0.U),
        protoTx.Lit(_.en -> true.B, _.input -> 100.U, _.output -> 0.U),
        protoTx.Lit(_.en -> true.B, _.input -> 1.U, _.output -> 0.U)
      )

      // Poking a bundle that has Input and Output
      for (t <- inputTransactions) {
        c.io.poke(t)
      }

      // No checking, just wanted to try poking a bundle
    }
  }
}