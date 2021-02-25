package verif

import chisel3._
import chisel3.util.DecoupledIO
import chisel3.experimental.BundleLiterals._
import chiseltest._

import scala.annotation.tailrec
import scala.util.Random

// I: interface type
// T: transaction type
// S: state type
// E: emission type
// TODO: break up this type into subsets for driver, monitor, and stim generator
trait TestComponent[I, T, S, E] {
  def getPokes(io: I, state: S): I // equivalent to UVM sequencer
  def update(io: I, state: S): S // internal state update after combinational nets resolve (before clock step)
  def emit(io: I, state: S): Seq[E] // equivalent to UVM monitor
  def genTxns(emit: Seq[E], state: S): Seq[T] // stimulus generator
  def newTxns(txns: Seq[T], state: S): S // equivalent to UVM sequencer
  def busy(io: I, state: S): Boolean // equivalent to UVM objection
}

object TestComponent {
  def runCycle[I <: Record, T, S, E](io: I, vip: TestComponent[I,T,S,E], state: S): (S, Seq[E]) = {
    val toPoke = vip.getPokes(io.peek(), state)
    io.pokePartial(toPoke)
    val peek = io.peek()
    val newState = vip.update(peek, state)
    val emission = vip.emit(peek, newState)
    val genTxns = vip.genTxns(emission, newState)
    val newStateAfterTx = vip.newTxns(genTxns, newState)
    (newStateAfterTx, emission)
  }

  def runSim[I <: Record, T, S, E](clock: Clock, io: I, vip: TestComponent[I,T,S,E], initState: S): Seq[E] = {
    runSimRec(clock, io, vip, initState, Seq.empty[E])
  }

  @tailrec
  final def runSimRec[I <: Record, T, S, E](clock: Clock, io: I, vip: TestComponent[I,T,S,E], state: S, emitted: Seq[E]): Seq[E] = {
    if (!vip.busy(io.peek(), state)) {
      emitted
    } else {
      val (newState, emission) = runCycle(io, vip, state)
      clock.step()
      runSimRec(clock, io, vip, newState, emitted ++ emission)
    }
  }
}

case class MasterState[T <: Data](txnsPending: Seq[DecoupledTX[T]], cycleCount: Int, totalCycles: Int)
object MasterState {
  def empty[T <: Data]: MasterState[T] = MasterState(Seq.empty[DecoupledTX[T]], 0, 0)
}

class DecoupledMaster[T <: Data](gen: T) extends TestComponent[DecoupledIO[T], DecoupledTX[T], MasterState[T], DecoupledTX[T]] {
  val protoIO = new DecoupledIO[T](gen)
  val protoTX = new DecoupledTX[T](gen)

  override def genTxns(emit: Seq[DecoupledTX[T]], state: MasterState[T]): Seq[DecoupledTX[T]] = Seq.empty

  override def newTxns(txns: Seq[DecoupledTX[T]], state: MasterState[T]): MasterState[T] = {
    state.copy(state.txnsPending ++ txns)
  }

  override def getPokes(io: DecoupledIO[T], state: MasterState[T]): DecoupledIO[T] = {
    if (state.txnsPending.isEmpty) {
      protoIO.Lit(_.valid -> false.B)
    } else {
      val nextTxn = state.txnsPending.head
      if (nextTxn.waitCycles.litValue <= state.cycleCount) {
        protoIO.Lit(_.valid -> true.B, _.bits -> nextTxn.data)
      } else {
        protoIO.Lit(_.valid -> false.B)
      }
    }
  }

  override def update(io: DecoupledIO[T], state: MasterState[T]): MasterState[T] = {
    if (io.valid.litToBoolean && io.ready.litToBoolean) { // transaction accepted
      state.copy(txnsPending = state.txnsPending.drop(1), cycleCount = 0, totalCycles = state.totalCycles + 1)
    } else {
      state.copy(cycleCount = state.cycleCount + 1, totalCycles = state.totalCycles + 1)
    }
  }

  override def emit(io: DecoupledIO[T], state: MasterState[T]): Seq[DecoupledTX[T]] = {
    if (io.valid.litToBoolean && io.ready.litToBoolean) {
      Seq(protoTX.Lit(_.data -> io.bits, _.cycleStamp -> state.totalCycles.U))
    } else {
      Seq.empty[DecoupledTX[T]]
    }
  }

  override def busy(io: DecoupledIO[T], state: MasterState[T]): Boolean = {
    state.txnsPending.nonEmpty
  }
}

case class SlaveState(totalCycles: Int, plannedBackpressure: Int, cyclesSinceValid: Int)
object SlaveState {
  def empty(firstBackpressure: Int = 0): SlaveState = SlaveState(0, firstBackpressure, 0)
}

trait Backpressure {
  def get: Int
}
class FixedBackpressure(backpressure: Int = 1) extends Backpressure {
  override def get: Int = backpressure
}
class RandomBackpressure(maxBackpressure: Int = 10, seed: Int = 1) extends Backpressure {
  val rand = new Random(seed)
  override def get: Int = {
    val x = rand.nextInt(maxBackpressure)
    println(x)
    x
  }
}

class DecoupledSlave[T <: Data](gen: T, backpressure: Backpressure) extends TestComponent[DecoupledIO[T], Nothing, SlaveState, DecoupledTX[T]] {
  override def genTxns(emit: Seq[DecoupledTX[T]], state: SlaveState): Seq[Nothing] = Seq.empty

  override def newTxns(txns: Seq[Nothing], state: SlaveState): SlaveState = state

  override def getPokes(io: DecoupledIO[T], state: SlaveState): DecoupledIO[T] = {
    if (io.valid.litToBoolean && state.plannedBackpressure <= state.cyclesSinceValid) {
      new DecoupledIO[T](gen).Lit(_.ready -> true.B)
    } else {
      new DecoupledIO[T](gen).Lit(_.ready -> false.B)
    }
  }

  override def update(io: DecoupledIO[T], state: SlaveState): SlaveState = {
    if (io.valid.litToBoolean && io.ready.litToBoolean) {
      state.copy(totalCycles = state.totalCycles + 1, plannedBackpressure = backpressure.get, cyclesSinceValid = 0)
    } else if (io.valid.litToBoolean) {
      state.copy(totalCycles = state.totalCycles + 1, cyclesSinceValid = state.cyclesSinceValid + 1)
    } else {
      state.copy(totalCycles = state.totalCycles + 1)
    }
  }

  override def emit(io: DecoupledIO[T], state: SlaveState): Seq[DecoupledTX[T]] = {
    if (io.ready.litToBoolean && io.valid.litToBoolean) {
      Seq(new DecoupledTX(gen).Lit(_.data -> io.bits, _.cycleStamp -> state.totalCycles.U))
    } else {
      Seq.empty
    }
  }

  override def busy(io: DecoupledIO[T], state: SlaveState): Boolean = false
}
