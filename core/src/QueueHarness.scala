package verif

import chisel3.util.QueueIO

class QueueHarness[T](gen: T) extends HarnessTop[QueueIO[T], DecoupledTX[T], DecoupledTX[T]] {
  val master = new DecoupledMaster(gen)
  val masterMon = new DecoupledMonitor(gen)
  val slave = new DecoupledSlave(gen, new FixedBackpressure(1))
  val slaveMon = new DecoupledMonitor(gen)

  override type S = this.type

  override def txns: Iterator[Seq[DecoupledTX[T]] => Seq[DecoupledTX[T]]] = ???

  override def newTxns(txns: Seq[DecoupledTX[T]], state: QueueHarness.this.type): QueueHarness.this.type = ???

  override def emit(io: QueueIO[T], state: QueueHarness.this.type): Seq[DecoupledTX[T]] = ???

  override def poke(io: QueueIO[T], state: QueueHarness.this.type): QueueIO[T] = ???

  override def update(io: QueueIO[T], state: QueueHarness.this.type): QueueHarness.this.type = ???

  override def busy(io: QueueIO[T], state: QueueHarness.this.type): Boolean = ???
}
