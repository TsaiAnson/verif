package verif

import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest._

import scala.annotation.tailrec

//State[S, I => O]
//trait State[I, S, O] {
  //def run(input: I, state: S): O
//}

// Ideas:
// wrap chiseltest test() as an object that can be peeked and poked in a specific order, but only once, and can only be stepped externally
// potentially control combinational loops some other way (through principled IO = peek, (IO => poke), (peek => S), step routine)
// def cycle[I <: Record](io: I, poker: I => I, ): ()

trait Objection[E, S] {
  def init: S
  def update(emission: E, state: S): S
  def busy(state: S): Boolean
}

object Objection {
  case class Num(numSeen: Int)
  def waitForN[E](num: Int, valid: E => Boolean): Objection[E, Num] = {
    new Objection[E, Num] {
      def init: Num = Num(0)

      def update(emission: E, state: Num): Num = state.copy(numSeen = if (valid(emission)) state.numSeen + 1 else state.numSeen)

      def busy(state: Num): Boolean = state.numSeen < num
    }
  }
}

trait StimulusGenerator[T, E, S] {
  def newStim(emission: E, state: S): (Seq[T], S)
  def init: S
  def done(state: S): Boolean
}

object StimulusGenerator {
  case class OneShotState(sent: Boolean)
  def oneShot[T, E](stim: Seq[T]): StimulusGenerator[T, E, OneShotState] = {
    new StimulusGenerator[T, E, OneShotState] { // remove E
      override def newStim(emission: E, state: OneShotState): (Seq[T], OneShotState) = (stim, state.copy(sent = true))

      override def init: OneShotState = OneShotState(false)

      override def done(state: OneShotState): Boolean = state.sent
    }
  }
}

// I: interface type
// T: transaction type
// S: state type
// E: emission type
trait TestComponent[I, T, S, +E] {
  def init: S
  def newTxns(txns: Seq[T], state: S): S // equivalent to UVM sequencer
  def getPokes(io: I, state: S): I // equivalent to UVM sequencer
  def update(io: I, state: S): S // internal state update after combinational nets resolve (before clock step)
  def emit(io: I, state: S): E // equivalent to UVM monitor
  def busy(io: I, state: S): Boolean // equivalent to UVM objection
}

object TestComponent {
  def runSim[I <: Record, T, S, E, SS, SE](clock: Clock, io: I, vip: TestComponent[I,T,S,E], stim: StimulusGenerator[T, E, SS], obj: Objection[E, SE]): Seq[E] = {
    runSimRec(clock, io, vip, vip.init, stim, stim.init, Seq.empty[E], obj, obj.init)
  }

  @tailrec
  final def runSimRec[I <: Record, T, S, E, SS, SE](clock: Clock, io: I, vip: TestComponent[I,T,S,E], state: S, stim: StimulusGenerator[T, E, SS], stimState: SS, emitted: Seq[E], obj: Objection[E, SE], objState: SE): Seq[E] = {
    if (!vip.busy(io.peek(), state) && stim.done(stimState) && !obj.busy(objState)) {
      emitted
    } else {
      val (newState, emission, newStimState) = runCycle(io, vip, state, stim, stimState)
      clock.step()
      val newObjState = obj.update(emission, objState)
      runSimRec(clock, io, vip, newState, stim, newStimState, emitted :+ emission, obj, newObjState)
    }
  }

  def runCycle[I <: Record, T, S, E, SS](io: I, vip: TestComponent[I,T,S,E], state: S, stimGen: StimulusGenerator[T, E, SS], stimState: SS): (S, E, SS) = {
    val toPoke = vip.getPokes(io.peek(), state)
    io.pokePartial(toPoke)
    val peek = io.peek() // TODO: fixpoint iteration
    val newState = vip.update(peek, state)
    val emission = vip.emit(peek, newState)
    if (stimGen.done(stimState)) {
      (newState, emission, stimState)
    } else {
      val (stim, newStimState) = stimGen.newStim(emission, stimState)
      val newStateAfterTx = vip.newTxns(stim, newState)
      (newStateAfterTx, emission, newStimState)
    }
  }

  // Can this be generalized for multiple FullComponents, each dispatched to one subset of I?
  // Probably better to stick with manual construction of the top-level test harness.
  def combine[I <: Record, I1 <: Data, I2 <: Data, T1, T2, S1, S2, E1, E2](
    one: TestComponent[I1, T1, S1, E1],
    oneMap: I => I1,
    two: TestComponent[I2, T2, S2, E2],
    twoMap: I => I2): TestComponent[I, (T1, T2), (S1, S2), (E1, E2)] = {
    new TestComponent[I, (T1, T2), (S1, S2), (E1, E2)] {
      def init: (S1, S2) = (one.init, two.init)

      def newTxns(txns: Seq[(T1, T2)], state: (S1, S2)): (S1, S2) = (one.newTxns(txns.map(_._1), state._1), two.newTxns(txns.map(_._2), state._2))

      def getPokes(io: I, state: (S1, S2)): I = io.cloneType.Lit(i => oneMap(i) -> one.getPokes(oneMap(io), state._1), i => twoMap(i) -> two.getPokes(twoMap(io), state._2))

      def emit(io: I, state: (S1, S2)): (E1, E2) = (one.emit(oneMap(io), state._1), two.emit(twoMap(io), state._2))

      def update(io: I, state: (S1, S2)): (S1, S2) = (one.update(oneMap(io), state._1), two.update(twoMap(io), state._2))

      def busy(io: I, state: (S1, S2)): Boolean = one.busy(oneMap(io), state._1) || two.busy(twoMap(io), state._2)
    }
  }
}
