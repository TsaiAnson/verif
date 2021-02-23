package verif

import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import chisel3.util.{DecoupledIO, Queue}
import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation

import scala.util.Random

// I: interface type
// T: transaction type
// S: state type
// E: emission type
trait TestComponent[I, T, S, E] {
  def newTxns(txns: Seq[T], state: S): S // equivalent to UVM sequencer
  def getPokes(io: I, state: S): I // equivalent to UVM driver
  def update(io: I, state: S): S // internal state update after combinational nets resolve (before clock step)
  def emit(io: I, state: S): Seq[E] // equivalent to UVM monitor
  def busy(io: I, state: S): Boolean // equivalent to UVM objection
}

case class MasterState[T <: Data](txnsPending: Seq[DecoupledTX[T]], cycleCount: Int, totalCycles: Int)
object MasterState {
  def empty[T <: Data]: MasterState[T] = MasterState(Seq.empty[DecoupledTX[T]], 0, 0)
  def stim[T <: Data](stim: Seq[DecoupledTX[T]]): MasterState[T] = MasterState(stim, 0, 0)
}

class DecoupledMaster[T <: Data](gen: T) extends TestComponent[DecoupledIO[T], DecoupledTX[T], MasterState[T], DecoupledTX[T]] {
  val protoIO = new DecoupledIO[T](gen)
  val protoTX = new DecoupledTX[T](gen)

  def newTxns(txns: Seq[DecoupledTX[T]], state: MasterState[T]): MasterState[T] = {
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


class Playground extends AnyFlatSpec with ChiselScalatestTester {
  def runTest[T <: MultiIOModule](c: T) = {

  }
  it should "run" in {
    val gen = UInt(8.W)
    val dut = () => new Queue(gen, 8, false, false)
    val master = new DecoupledMaster(gen)
    val slave = new DecoupledSlave(gen, new FixedBackpressure(2))
    val stim = Seq.tabulate(8)(i => new DecoupledTX[UInt](gen).tx((1+i).U, i, 0))
    var masterState = MasterState.stim[UInt](stim)
    var slaveState = SlaveState.empty()

    test(dut()).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      do {
        masterState = master.newTxns(Seq.empty, masterState)
        slaveState = slave.newTxns(Seq.empty, slaveState)

        val toPokeMaster = master.getPokes(c.io.enq.peek(), masterState)
        val toPokeSlave = slave.getPokes(c.io.deq.peek(), slaveState)
        c.io.enq.pokePartial(toPokeMaster)
        c.io.deq.pokePartial(toPokeSlave)

        masterState = master.update(c.io.enq.peek(), masterState)
        slaveState = slave.update(c.io.deq.peek(), slaveState)

        master.emit(c.io.enq.peek(), masterState).foreach(println("Master", _))
        slave.emit(c.io.deq.peek(), slaveState).foreach(println("Slave", _))

        c.clock.step()
      } while (master.busy(c.io.enq.peek(), masterState) || slave.busy(c.io.deq.peek(), slaveState))
    }
    // TODO: make stimulus a special test component type (takes things it needs and produces a new stim each cycle and can tell when it is finished)
  }
}
