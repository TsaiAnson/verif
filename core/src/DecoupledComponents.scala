package verif

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util.DecoupledIO

case class DecoupledMasterTX[T <: Data](gen: T) extends Bundle {
  val data: T = gen.cloneType
  val waitCycles: UInt = UInt(32.W)
  //val postSendCycles: UInt = UInt(32.W)
  def tx(data: T, waitCycles: Int = 0): DecoupledMasterTX[T] = {
    this.Lit(_.data -> data, _.waitCycles -> waitCycles.U)
  }
}

case class DecoupledMonTX[T <: Data](gen: T) extends Bundle {
  val data: T = gen.cloneType
  val cycleStamp: UInt = UInt(32.W)
  def tx(data: T, cycleStamp: Int): DecoupledMonTX[T] = {
    this.Lit(_.data -> data, _.cycleStamp -> cycleStamp.U)
  }
}

class DecoupledSlaveTX extends Bundle {
  val backpressure: UInt = UInt(8.W)
}
object DecoupledSlaveTX {
  def apply(backpressure: Int): DecoupledSlaveTX = (new DecoupledSlaveTX).Lit(_.backpressure -> backpressure.U)
}

case class MasterState[T <: Data](txnsPending: Seq[DecoupledMasterTX[T]], cycleCount: Int, totalCycles: Int)

class DecoupledMaster[T <: Data](gen: T) extends
  TestComponent[DecoupledIO[T], DecoupledMasterTX[T], MasterState[T], Option[DecoupledMonTX[T]]] {
  val protoIO = new DecoupledIO[T](gen)
  val protoTX = new DecoupledMonTX[T](gen)

  def init: MasterState[T] = MasterState(Seq.empty[DecoupledMasterTX[T]], 0, 0)

  def newTxns(txns: Seq[DecoupledMasterTX[T]], state: MasterState[T]): MasterState[T] = {
    state.copy(txnsPending = state.txnsPending ++ txns)
  }

  def getPokes(peeked: DecoupledIO[T], state: MasterState[T]): DecoupledIO[T] = {
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

  def update(peeked: DecoupledIO[T], state: MasterState[T]): MasterState[T] = {
    if (peeked.valid.litToBoolean && peeked.ready.litToBoolean) { // transaction accepted
      state.copy(txnsPending = state.txnsPending.drop(1), cycleCount = 0, totalCycles = state.totalCycles + 1)
    } else {
      state.copy(cycleCount = state.cycleCount + 1, totalCycles = state.totalCycles + 1)
    }
  }

  def emit(io: DecoupledIO[T], state: MasterState[T]): Option[DecoupledMonTX[T]] = {
    if (io.valid.litToBoolean && io.ready.litToBoolean) {
      Some(protoTX.tx(io.bits, state.totalCycles))
    } else {
      None
    }
  }

  def busy(io: DecoupledIO[T], state: MasterState[T]): Boolean = {
    state.txnsPending.nonEmpty
  }
}

case class SlaveState(currentBackpressure: Int, cyclesSinceValid: Int, totalCycles: Int)

class DecoupledSlave[T <: Data](gen: T, initialBackpressure: Int = 0) extends
  TestComponent[DecoupledIO[T], DecoupledSlaveTX, SlaveState, Option[DecoupledMonTX[T]]] {
  val protoIO = new DecoupledIO[T](gen)
  val protoTX = new DecoupledMonTX[T](gen)

  def init: SlaveState = SlaveState(initialBackpressure, 0, 0)

  def newTxns(txns: Seq[DecoupledSlaveTX], state: SlaveState): SlaveState = {
    state.copy(currentBackpressure = txns.last.backpressure.litValue.toInt)
  }

  def getPokes(io: DecoupledIO[T], state: SlaveState): DecoupledIO[T] = {
    if (io.valid.litToBoolean && state.currentBackpressure <= state.cyclesSinceValid) {
      protoIO.Lit(_.ready -> true.B)
    } else {
      protoIO.Lit(_.ready -> false.B)
    }
  }

  override def update(io: DecoupledIO[T], state: SlaveState): SlaveState = {
    if (io.valid.litToBoolean && io.ready.litToBoolean) {
      state.copy(cyclesSinceValid = 0, totalCycles = state.totalCycles + 1)
    } else if (io.valid.litToBoolean) {
      state.copy(cyclesSinceValid = state.cyclesSinceValid + 1, totalCycles = state.totalCycles + 1)
    } else {
      state.copy(totalCycles = state.totalCycles + 1)
    }
  }

  // TODO: Duplicated code: extract emit on an interface into a typeclass
  def emit(io: DecoupledIO[T], state: SlaveState): Option[DecoupledMonTX[T]] = {
    if (io.valid.litToBoolean && io.ready.litToBoolean) {
      Some(protoTX.tx(io.bits, state.totalCycles))
    } else {
      None
    }
  }

  override def busy(io: DecoupledIO[T], state: SlaveState): Boolean = false
}
