package verif

import chisel3._
import chisel3.util.DecoupledIO
import chisel3.experimental.BundleLiterals._
import chiseltest._

import scala.annotation.tailrec
import scala.util.Random

sealed trait BaseComponent[I] {
  type S
  def update(io: I, state: S): S // state update (before clock step)
}

trait ActiveComponent[I, T] extends BaseComponent[I] {
  type S
  def newTxns(txns: Seq[T], state: S): S // sequencer
  def poke(io: I, state: S): I // driver
  def busy(io: I, state: S): Boolean // equivalent to UVM objection
}

trait PassiveComponent[I, E] extends BaseComponent[I] {
  type S
  def emit(io: I, state: S): E // monitor
}

trait FullComponent[I, T, E] extends ActiveComponent[I, T] with PassiveComponent[I, E]

object TestComponent {
  def combine[I, T, E](active: ActiveComponent[I, T], passive: PassiveComponent[I, E]): FullComponent[I, T, E] = {
    new FullComponent[I, T, E] {
      override type S = (active.S, passive.S)
      override def newTxns(txns: Seq[T], state: S): S = (active.newTxns(txns, state._1), state._2)
      override def poke(io: I, state: S): I = active.poke(io, state._1)
      override def emit(io: I, state: S): E = passive.emit(io, state._2)
      override def update(io: I, state: S): S = (active.update(io, state._1), passive.update(io, state._2))
      override def busy(io: I, state: S): Boolean = active.busy(io, state._1)
    }
  }

  // Can this be generalized for multiple FullComponents, each dispatched to one subset of I?
  // Probably better to stick with manual construction of the top-level test harness.
  def combine[I <: Record, I1 <: Data, I2 <: Data, T1, T2, E1, E2](one: FullComponent[I1, T1, E1], oneMap: I => I1, two: FullComponent[I2, T2, E2], twoMap: I => I2): FullComponent[I, (T1, T2), (E1, E2)] = {
    new FullComponent[I, (T1, T2), (E1, E2)] {
      override type S = (one.S, two.S)

      override def newTxns(txns: Seq[(T1, T2)], state: S): S = (one.newTxns(txns.map(_._1), state._1), two.newTxns(txns.map(_._2), state._2))

      override def poke(io: I, state: S): I = io.cloneType.Lit(i => oneMap(i) -> one.poke(oneMap(io), state._1), i => twoMap(i) -> two.poke(twoMap(io), state._2))

      override def emit(io: I, state: S): (E1, E2) = (one.emit(oneMap(io), state._1), two.emit(twoMap(io), state._2))

      override def update(io: I, state: S): S = (one.update(oneMap(io), state._1), two.update(twoMap(io), state._2))

      override def busy(io: I, state: S): Boolean = one.busy(oneMap(io), state._1) || two.busy(twoMap(io), state._2)
    }
  }
}

/*
trait HarnessTop[I, T, E] extends BaseComponent[I] with GetsTransactions[T] with EmitsTransactions[I, E] {
  type S
  def txns: Iterator[Seq[E] => Seq[T]]
  //def genTxns(state: S, emission: Seq[E]): Seq[T]
  // stim: Iterator[Seq[DecoupledTX[T]]]
}
 */




class DecoupledMaster[T <: Data](gen: T) extends ActiveComponent[DecoupledIO[T], DecoupledTX[T]] {
  case class State(txnsPending: Seq[DecoupledTX[T]], cycleCount: Int)
  object State {
    def empty: State = State(Seq.empty[DecoupledTX[T]], 0)
  }
  override type S = State

  val protoIO = new DecoupledIO[T](gen)

  override def newTxns(txns: Seq[DecoupledTX[T]], state: State): State = {
    state.copy(txnsPending = state.txnsPending ++ txns)
  }

  override def poke(io: DecoupledIO[T], state: State): DecoupledIO[T] = {
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

  override def update(io: DecoupledIO[T], state: State): State = {
    if (io.valid.litToBoolean && io.ready.litToBoolean) { // transaction accepted
      state.copy(txnsPending = state.txnsPending.drop(1), cycleCount = 0)
    } else {
      state.copy(cycleCount = state.cycleCount + 1)
    }
  }

  override def busy(io: DecoupledIO[T], state: State): Boolean = {
    state.txnsPending.nonEmpty
  }
}

class DecoupledMonitor[T <: Data](gen: T) extends PassiveComponent[DecoupledIO[T], Option[DecoupledTX[T]]] {
  case class State(totalCycles: Int)
  override type S = State

  val protoTX = new DecoupledTX[T](gen)
  override def emit(io: DecoupledIO[T], state: State): Option[DecoupledTX[T]] = {
    if (io.valid.litToBoolean && io.ready.litToBoolean) {
      Some(protoTX.Lit(_.data -> io.bits, _.cycleStamp -> state.totalCycles.U))
    } else {
      None
    }
  }

  override def update(io: DecoupledIO[T], state: State): State = {
    state.copy(totalCycles = state.totalCycles + 1)
  }
}

/*
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
    x
  }
}
 */

case class Backpressure() extends Bundle {
  val cycles = UInt(32.W)
}
object Backpressure {
  def tx(backpressure: Int): Backpressure = {
    (new Backpressure).Lit(_.cycles -> backpressure.U)
  }
}

class DecoupledSlave[T <: Data](gen: T, initialBackpressure: Int = 0) extends ActiveComponent[DecoupledIO[T], Backpressure] {
  case class State(currentBackpressure: Int, cyclesSinceValid: Int)
  object State {
    def empty: State = State(initialBackpressure, 0)
  }
  override type S = State

  val protoIO = new DecoupledIO[T](gen)

  override def newTxns(txns: Seq[Backpressure], state: State): State = {
    state.copy(currentBackpressure = txns.last.cycles.litValue.toInt)
  }

  override def poke(io: DecoupledIO[T], state: State): DecoupledIO[T] = {
    if (io.valid.litToBoolean && state.currentBackpressure <= state.cyclesSinceValid) {
      protoIO.Lit(_.ready -> true.B)
    } else {
      protoIO.Lit(_.ready -> false.B)
    }
  }

  override def update(io: DecoupledIO[T], state: State): State = {
    if (io.valid.litToBoolean && io.ready.litToBoolean) {
      state.copy(cyclesSinceValid = 0)
    } else if (io.valid.litToBoolean) {
      state.copy(cyclesSinceValid = state.cyclesSinceValid + 1)
    } else {
      state
    }
  }

  override def busy(io: DecoupledIO[T], state: State): Boolean = false
}

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

  // wrap chiseltest test() as an object that can be peeked and poked in a specific order, but only once, and can only be stepped externally
  // potentially control combinational loops some other way (through principled IO = peek, (IO => poke), (peek => S), step routine)
  //def cycle[I <: Record](io: I, poker: I => I, ): ()
}

/*
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


 */