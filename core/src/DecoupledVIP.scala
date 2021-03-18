package verif

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.{DataMirror, Direction}
import chiseltest._
import scala.collection.mutable

class DecoupledTX[T <: Data](gen: T) extends Bundle {
  val data: T = gen.cloneType
  // TODO: move these meta fields into typeclasses that can be mixed in with DecoupledTX
  val waitCycles: UInt = UInt(32.W)
  val postSendCycles: UInt = UInt(32.W)
  val cycleStamp: UInt = UInt(32.W)

  // TODO: split into driver and monitor TXs
  // TODO: how can we check that data: T fits into gen? (e.g. gen = UInt(2.W), data = 16.U shouldn't work)
  def tx(data: T, waitCycles: Int, postSendCycles: Int): DecoupledTX[T] = {
    this.Lit(_.data -> data, _.waitCycles -> waitCycles.U, _.postSendCycles -> postSendCycles.U, _.cycleStamp -> 0.U)
  }
  def tx(data: T): DecoupledTX[T] = {
    this.Lit(_.data -> data, _.waitCycles -> 0.U, _.postSendCycles -> 0.U)
  }
  def tx(data: T, cycleStamp: Int): DecoupledTX[T] = {
    this.Lit(_.data -> data, _.cycleStamp -> cycleStamp.U)
  }

  override def cloneType: this.type = (new DecoupledTX(gen)).asInstanceOf[this.type]
}

// TODO: combine driver and monitor into VIP/Agent to keep API clean
// TODO: VIP/Agent should have master and slave modes (monitor should never peek)
class DecoupledDriverMaster[T <: Data](clock: Clock, interface: DecoupledIO[T]) {
  assert(DataMirror.directionOf(interface.valid) == Direction.Input, "DecoupledDriverMaster is connected to a master port, not a slave")
  val inputTransactions: mutable.Queue[DecoupledTX[T]] = mutable.Queue[DecoupledTX[T]]()
  fork.withRegion(TestdriverMain) {
    var cycleCount = 0
    var idleCycles = 0
    interface.valid.poke(false.B)
    while (true) {
      if (inputTransactions.nonEmpty && idleCycles == 0) {
        val t = inputTransactions.dequeue()
        if (t.waitCycles.litValue().toInt > 0) {
          idleCycles = t.waitCycles.litValue().toInt
          while (idleCycles > 0) {
            idleCycles -= 1
            cycleCount += 1
            clock.step()
          }
        }
        while (!interface.ready.peek().litToBoolean) {
          cycleCount += 1
          clock.step()
        }

        cycleCount += 1
        timescope { // TODO: why do we need a new timescope, can we force valid to false later explicitly?
          t.data match {
            case bundle: Bundle =>
              interface.bits.asInstanceOf[Bundle].pokePartial(bundle)
              // TODO: why is this special cased?
//              interface.bits.poke(t.data)
            case _ =>
              interface.bits.poke(t.data)
          }
          interface.valid.poke(true.B)
          clock.step()
        }

        idleCycles = t.postSendCycles.litValue().toInt
      } else {
        if (idleCycles > 0) idleCycles -= 1
        cycleCount += 1
        clock.step()
      }
    }
  }

  def push(txn: DecoupledTX[T]): Unit = {
    inputTransactions += txn
  }

  def push(txns: Seq[DecoupledTX[T]]): Unit = {
    for (t <- txns) {
      inputTransactions += t
    }
  }
}

// TODO: have this return a stream of seen transactions
class DecoupledDriverSlave[T <: Data](clock: Clock, interface: DecoupledIO[T], waitCycles: Int = 0) {
  assert(DataMirror.directionOf(interface.valid) == Direction.Output, "DecoupledDriverSlave is connected to a slave port, not a master")
  fork.withRegion(TestdriverMain) {
    var cycleCount = 0
    var idleCyclesD = 0
    while (true) {
      interface.ready.poke(false.B)
      while (idleCyclesD > 0) {
        idleCyclesD -= 1
        cycleCount += 1
        clock.step()
      }
      interface.ready.poke(true.B)
      if (interface.valid.peek().litToBoolean) {
        idleCyclesD = waitCycles
      }
      cycleCount += 1
      clock.step()
    }
  }
}

class DecoupledMonitor[T <: Data](clock: Clock, interface: DecoupledIO[T]) {
  val monitoredTransactions: mutable.Queue[DecoupledTX[T]] = mutable.Queue[DecoupledTX[T]]()
  fork.withRegion(Monitor) {
    var cycleCount = 0
    while (true) {
      if (interface.valid.peek().litToBoolean && interface.ready.peek().litToBoolean) {
        val t = new DecoupledTX(interface.bits.cloneType.asInstanceOf[T]) // asInstanceOf[T] to make IntelliJ happy
        val tLit = t.Lit(_.data -> interface.bits.peek(), _.cycleStamp -> cycleCount.U)
        monitoredTransactions += tLit
      }
      cycleCount += 1
      clock.step()
    }
  }

  def clearMonitoredTransactions(): Unit = {
    monitoredTransactions.clear()
  }
}
