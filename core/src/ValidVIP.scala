package verif

import chisel3._
import chisel3.util._
import chiseltest._
import scala.collection.mutable.{MutableList, Queue}

case class ValidTX[T <: Data](data: T, waitCycles: UInt = 0.U, postSendCycles: UInt = 0.U, cycleStamp: Int = 0) extends Bundle {
  override def cloneType = ValidTX(data, waitCycles, postSendCycles).asInstanceOf[this.type]
}

class ValidDriver[T <: Data](clock: Clock, interface: ValidIO[T]) extends
  AbstractDriver[ValidIO[T], ValidTX[T]](clock, interface) {

  fork {
    var cycleCount = 0
    var idleCycles = 0
    while (true) {
      if (hasNextTransaction && idleCycles == 0) {
        val t = getNextTransaction
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
}

class ValidMonitor[T <: Data](clock: Clock, interface: ValidIO[T]) extends
  AbstractMonitor[ValidIO[T], ValidTX[T]](clock, interface) {
  fork {
    var cycleCount = 0
    while (true) {
      if (interface.valid.peek().litToBoolean) {
        val t = ValidTX[T](interface.bits.peek(), cycleStamp = cycleCount)
        addMonitoredTransaction(t)
      }
      cycleCount += 1
      clock.step()
    }
  }
}
