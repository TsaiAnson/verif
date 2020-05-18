package verif

import org.scalatest._
import chisel3._
import chiseltest._
import chisel3.util._
import chiseltest.experimental.TestOptionBuilder._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.DataMirror._
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}


// Defining module here for simplicity
case class MultiDirBundleIO() extends Bundle {
  val en = Input(Bool())
  val input = Input(UInt(8.W))
  val dummy = Input(UInt(8.W))
  val output = Output(UInt(8.W))
}

class MultiDirBundleIOModule extends MultiIOModule {
  val io = IO(new MultiDirBundleIO)

  io.output := io.dummy
}

class MultiDirBundlePokeTest extends FlatSpec with ChiselScalatestTester {
  it should "multi dir bundle poke test" in {
    test(new MultiDirBundleIOModule).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>

      val protoTx = MultiDirBundleIO()
      val inputTransactions = Seq(
        protoTx.Lit(_.en -> false.B, _.input -> 9.U,  _.dummy -> 255.U,_.output -> 255.U),
        protoTx.Lit(_.en -> true.B, _.input -> 100.U,  _.dummy -> 255.U,_.output -> 255.U),
        protoTx.Lit(_.en -> true.B, _.input -> 1.U, _.dummy -> 255.U, _.output -> 255.U)
      )

      // Poking a bundle that has Input and Output
      for (t <- inputTransactions) {
        c.io.poke(t)
      }

      // No checking, just wanted to try poking a bundle
    }
  }

  it should "data mirror multi dir bundle poke test" in {
    test(new MultiDirBundleIOModule).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>

      val protoTx = MultiDirBundleIO()
      val inputTransactions = Seq(
//        protoTx.Lit(_.en -> false.B, _.input -> 9.U, _.output -> 255.U),
//        protoTx.Lit(_.en -> true.B, _.input -> 100.U, _.output -> 255.U),
//        protoTx.Lit(_.en -> true.B, _.input -> 1.U, _.output -> 255.U)
        protoTx.Lit(_.en -> false.B, _.input -> 9.U,  _.dummy -> 255.U,_.output -> 255.U),
        protoTx.Lit(_.en -> true.B, _.input -> 100.U,  _.dummy -> 255.U,_.output -> 255.U),
        protoTx.Lit(_.en -> true.B, _.input -> 1.U, _.dummy -> 255.U, _.output -> 255.U)
      )

      // Poking a bundle that has Input and Output using Data Mirror
      for (t <- inputTransactions) {
        for (p <- c.io.getElements) {
          if (directionOf(p) == ActualDirection.Input) {
            for (d <- t.getElements) {
              // DOES NOT WORK FOR MULTIPLE INPUTS WITH SAME TYPE, could look into reflection
              if (p.getClass == d.getClass && directionOf(d) == ActualDirection.Input) {
                p.poke(d)
              }
            }
          }
        }
        c.clock.step()
        println(c.io.output.peek().litValue())
      }

      // No checking, just wanted to try poking a bundle
    }
  }
}