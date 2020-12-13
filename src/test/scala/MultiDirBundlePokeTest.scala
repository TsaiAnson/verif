package verif

import java.lang.reflect.Field

import org.scalatest._
import chisel3._
import chiseltest._
import chisel3.util._
import chiseltest.experimental.TestOptionBuilder._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.DataMirror._
import chiseltest.internal.VerilatorBackendAnnotation


// Defining module here for simplicity
case class MultiDirBundleIO() extends Bundle {
  val en = Input(Bool())
  val input = Input(UInt(8.W))
  // Dummy is another input that does nothing
  val dummy = Input(UInt(8.W))
  val output = Output(UInt(8.W))
}

class MultiDirBundleIOModule extends MultiIOModule {
  val io = IO(new MultiDirBundleIO)

  io.output := io.input
}

class MultiDirBundlePokeTest extends FlatSpec with ChiselScalatestTester {
  // Cannot poke duplex bundle using "poke"
  it should "fail multi dir bundle poke test" in {
    assertThrows[chiseltest.UnpokeableException] {
      test(new MultiDirBundleIOModule).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>

        val protoTx = MultiDirBundleIO()
        val inputTransactions = Seq(
          protoTx.Lit(_.en -> false.B, _.input -> 9.U, _.dummy -> 254.U, _.output -> 255.U),
          protoTx.Lit(_.en -> true.B, _.input -> 100.U, _.dummy -> 254.U, _.output -> 255.U),
          protoTx.Lit(_.en -> true.B, _.input -> 1.U, _.dummy -> 254.U, _.output -> 255.U)
        )

        // Poking a bundle that has Input and Output
        for (t <- inputTransactions) {
          c.io.poke(t)
        }

        // No checking, just wanted to try poking a bundle
      }
    }
  }

  // Can poke duplex bundle using data mirror (optimized with caching input fields)
  it should "data mirror multi dir bundle poke test" in {
    test(new MultiDirBundleIOModule).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>

      val protoTx = MultiDirBundleIO()
      val inputTransactions = Seq(
        protoTx.Lit(_.en -> false.B, _.input -> 9.U,  _.dummy -> 254.U,_.output -> 255.U),
        protoTx.Lit(_.en -> true.B, _.input -> 100.U,  _.dummy -> 254.U,_.output -> 255.U),
        protoTx.Lit(_.en -> true.B, _.input -> 1.U, _.dummy -> 254.U, _.output -> 255.U)
      )

      // Caching inputs
      val inputCache = collection.mutable.ListBuffer[Field]()
      for (f <- c.io.getClass.getDeclaredFields) {
        f.setAccessible(true)
        if (directionOf(f.get(c.io).asInstanceOf[Data]) == ActualDirection.Input) {
          inputCache += f
        }
      }

      // Poking
      for (t <- inputTransactions) {
        for (f <- inputCache) {
          f.get(c.io).asInstanceOf[Data].poke(f.get(t).asInstanceOf[Data])
        }
        println(c.io.output.peek().litValue())
      }
    }
  }

  // Can poke using pokePartial implementation (which uses data mirror under the hood)
  it should "partial poke multi dir bundle poke test" in {
    test(new MultiDirBundleIOModule).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>

      val protoTx = MultiDirBundleIO()
      val inputTransactions = Seq(
        protoTx.Lit(_.en -> false.B, _.input -> 9.U,  _.dummy -> 254.U,_.output -> 255.U),
        protoTx.Lit(_.en -> true.B, _.input -> 100.U,  _.dummy -> 254.U,_.output -> 255.U),
        protoTx.Lit(_.en -> true.B, _.input -> 1.U, _.dummy -> 254.U, _.output -> 255.U)
      )

      // Poking
      for (t <- inputTransactions) {
        c.io.pokePartial(t)
        println(c.io.output.peek().litValue())
      }
    }
  }
}