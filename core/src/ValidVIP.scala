package verif

import chisel3._
import chisel3.util._
import chiseltest._
import scala.collection.mutable

case class ValidTX[T <: Data](data: T, waitCycles: UInt = 0.U, postSendCycles: UInt = 0.U, cycleStamp: Int = 0) extends Bundle {
  override def cloneType = ValidTX(data, waitCycles, postSendCycles).asInstanceOf[this.type]
}

class ValidDriver[T <: Data](clock: Clock, interface: ValidIO[T]) {
  val inputTransactions: mutable.Queue[ValidTX[T]] = mutable.Queue[ValidTX[T]]()
  fork {
    var cycleCount = 0
    var idleCycles = 0
    while (true) {
      if (inputTransactions.nonEmpty && idleCycles == 0) {
        val t = inputTransactions.dequeue()
        if (t.waitCycles.litValue().toInt > 0) {
          idleCycles = t.waitCycles.litValue().toInt
          while (idleCycles > 0) {
            // For debugging use
            // println("IDUT", cycleCount)
            idleCycles -= 1
            cycleCount += 1
            clock.step()
          }
        }

        cycleCount += 1
        timescope {
          if (t.data.isInstanceOf[Bundle]) {
            interface.bits.asInstanceOf[Bundle].pokePartial(t.data.asInstanceOf[Bundle])
          } else {
            interface.bits.poke(t.data)
          }
          interface.valid.poke(true.B)
          clock.step()
        }

        idleCycles = t.postSendCycles.litValue().toInt
      } else {
        if (idleCycles > 0) idleCycles -= 1
        cycleCount += 1
        // For debugging use
        // println("IDUT", cycleCount)
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
  fork {
    var cycleCount = 0
    while (true) {
      if (interface.valid.peek().litToBoolean) {
        val t = ValidTX[T](interface.bits.peek(), cycleStamp = cycleCount)
        monitoredTransactions += t
      }
      cycleCount += 1
      clock.step()
    }
  }

  def clearMonitoredTransactions(): Unit = {
    monitoredTransactions.clear()
  }
}
