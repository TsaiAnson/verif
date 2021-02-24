package verif

import chisel3._
import chisel3.util.QueueIO
import chisel3.experimental.BundleLiterals._

case class QueueTesterState[T <: Data](mState: MasterState[T], sState: SlaveState, outputsSeen: Int)
object QueueTesterState {
  def withStim[T <: Data](gen: T, stim: Int => T): QueueTesterState[T] = {
    val s = Seq.tabulate(8)(i => new DecoupledTX[T](gen).tx(stim(i), 1 + i*2, 0))
    QueueTesterState(MasterState.stim[T](s), SlaveState.empty(), 0)
  }
}
case class QueueTesterEmission[T <: Data](masterPort: Option[DecoupledTX[T]], slavePort: Option[DecoupledTX[T]])

// One idea: compose 2 test components into a larger one that encompasses both interfaces!
// TODO: make stimulus a special test component type (takes things it needs and produces a new stim each cycle and can tell when it is finished)
class QueueTester[T <: UInt](gen: T) extends TestComponent[QueueIO[T], Nothing, QueueTesterState[T], QueueTesterEmission[T]] {
  val master = new DecoupledMaster(gen)
  val slave = new DecoupledSlave(gen, new FixedBackpressure(2))

  override def newTxns(txns: Seq[Nothing], state: QueueTesterState[T]): QueueTesterState[T] = {
    state // TODO: implement runtime stimulus
  }

  override def getPokes(io: QueueIO[T], state: QueueTesterState[T]): QueueIO[T] = {
    val enqPokes = master.getPokes(io.enq, state.mState)
    val deqPokes = slave.getPokes(io.deq, state.sState)
    // TODO: pass IO proto from top-level
    new QueueIO[T](gen, 8).Lit(_.enq -> enqPokes, _.deq -> deqPokes)
  }

  override def update(io: QueueIO[T], state: QueueTesterState[T]): QueueTesterState[T] = {
    state.copy(
      mState = master.update(io.enq, state.mState),
      sState = slave.update(io.deq, state.sState),
      outputsSeen = if (io.deq.ready.litToBoolean && io.deq.valid.litToBoolean) state.outputsSeen + 1 else state.outputsSeen
    )
  }

  override def emit(io: QueueIO[T], state: QueueTesterState[T]): Seq[QueueTesterEmission[T]] = {
    Seq(QueueTesterEmission(
      master.emit(io.enq, state.mState).headOption,
      slave.emit(io.deq, state.sState).headOption
    ))
  }

  override def busy(io: QueueIO[T], state: QueueTesterState[T]): Boolean = {
    master.busy(io.enq, state.mState) || slave.busy(io.deq, state.sState) || state.outputsSeen < 8
  }
}
