package verif

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.{DataMirror, Direction}
import chiseltest._
import scala.collection.mutable

class ValidTX[T <: Data](gen: T) extends Bundle {
  val data: T = gen.cloneType
  // TODO: move these meta fields into typeclasses that can be mixed in with DecoupledTX
  val waitCycles: UInt = UInt(32.W)
  val postSendCycles: UInt = UInt(32.W)
  val cycleStamp: UInt = UInt(32.W)

  // TODO: split into driver and monitor TXs
  // TODO: how can we check that data: T fits into gen? (e.g. gen = UInt(2.W), data = 16.U shouldn't work)
  def tx(data: T, waitCycles: Int, postSendCycles: Int): ValidTX[T] = {
    this.Lit(_.data -> data, _.waitCycles -> waitCycles.U, _.postSendCycles -> postSendCycles.U, _.cycleStamp -> 0.U)
  }
  def tx(data: T): ValidTX[T] = {
    this.Lit(_.data -> data, _.waitCycles -> 0.U, _.postSendCycles -> 0.U)
  }
  def tx(data: T, cycleStamp: Int): ValidTX[T] = {
    this.Lit(_.data -> data, _.cycleStamp -> cycleStamp.U)
  }

  override def cloneType: this.type = (new ValidTX(gen)).asInstanceOf[this.type]
}

class ValidDriverMaster[T <: Data](clock: Clock, interface: ValidIO[T]) {
  assert(DataMirror.directionOf(interface.valid) == Direction.Input, "ValidDriverMaster is connected to a master port, not a slave")
  val inputTransactions: mutable.Queue[ValidTX[T]] = mutable.Queue[ValidTX[T]]()
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

        cycleCount += 1
        timescope {
          t.data match {
            case bundle: Bundle =>
              interface.bits.asInstanceOf[Bundle].pokePartial(bundle)
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

  def push(txn: ValidTX[T]): Unit = {
    inputTransactions += txn
  }

  def push(txns: Seq[ValidTX[T]]): Unit = {
    for (t <- txns) {
      inputTransactions += t
    }
  }
}

class ValidMonitor[T <: Data](clock: Clock, interface: ValidIO[T]) {
  val monitoredTransactions: mutable.Queue[ValidTX[T]] = mutable.Queue[ValidTX[T]]()
  fork.withRegion(Monitor) {
    var cycleCount = 0
    while (true) {
      if (interface.valid.peek().litToBoolean) {
        val t = new ValidTX(interface.bits.cloneType.asInstanceOf[T])
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
