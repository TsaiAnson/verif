package verif

import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import chisel3.util.{DecoupledIO, Queue, QueueIO}
import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation

class Playground extends AnyFlatSpec with ChiselScalatestTester {
  it should "run" in {
    val dut = () => new Queue(UInt(8.W), 8, false, false)

    trait TestComponent[I, T, S] {
      def getPokes(txns: Seq[T], state: S): (I, S)
      def update(io: I, state: S): S
      def busy(io: I, state: S): Boolean
    }

    case class MasterState[T](txnsPending: Seq[DecoupledTX[T]], cycleCount: Int)
    object MasterState {
      def apply[T]: MasterState[T] = MasterState(Seq.empty[DecoupledTX[T]], 0)
    }
    class DecoupledMaster[T](gen: T) extends TestComponent[DecoupledIO[T], DecoupledTX[T], MasterState[T]] {
      override def getPokes(txns: Seq[DecoupledTX[T]], state: MasterState[T]): (DecoupledIO[T], MasterState[T]) = {
        val txnsToSend = state.txnsPending ++ txns
        if (txnsToSend.isEmpty) {
          (DecoupledIO[T](gen).Lit(_.valid -> false.B), state.copy(cycleCount = 0))
        } else {
          val newState = state.copy(state.txnsPending ++ txns)

        }
      }

      override def update(io: DecoupledIO[T], state: MasterState[T]): MasterState[T] = ???

      override def busy(io: DecoupledIO[T], state: MasterState[T]): Boolean = ???
    }

    def finished(io: QueueIO[UInt]): Boolean = {
      io.deq.valid.litToBoolean
    }

    def getPokes(): QueueIO[UInt] = {
      val poke = new QueueIO[UInt](UInt(8.W), 8)
      val enqPoke = new DecoupledIO[UInt](UInt(8.W)).Lit(_.bits -> 100.U, _.valid -> true.B)
      val deqPoke = new DecoupledIO[UInt](UInt(8.W)).Lit(_.ready -> true.B)
      poke.Lit(_.enq -> enqPoke, _.deq -> deqPoke)
    }

    def update(io: QueueIO[UInt]): Unit = {
      // no state to update
    }

    test(dut()).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      while (!finished(c.io.peek())) {
        val pokes = getPokes()
        c.io.pokePartial(pokes)
        update(c.io.peek())
        c.clock.step()
        println("advanced clock")
      }
    }
  }
}
