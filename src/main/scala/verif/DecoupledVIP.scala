package verif

import chisel3._
import chisel3.util._
import chiseltest._
import scala.collection.mutable.{MutableList, Queue}

case class DecoupledTX[T <: Data](data: T, waitCycles: UInt = 0.U, postSendCycles: UInt = 0.U, cycleStamp: Int = 0) extends Bundle {
  override def cloneType = DecoupledTX(data, waitCycles, postSendCycles).asInstanceOf[this.type]
}

class DecoupledDriver[T <: Data](clock: Clock, interface: DecoupledIO[T]) extends
  AbstractDriver[DecoupledIO[T], DecoupledTX[T]](clock, interface) {
  fork {
    var cycleCount = 0
    var idleCycles = 0
    while (true) {
      if (hasNextTransaction() && idleCycles == 0) {
        val t = getNextTransaction()
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
        while (!interface.ready.peek().litToBoolean) {
          cycleCount += 1
          clock.step()
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

class DecoupledMonitor[T <: Data](clock: Clock, interface: DecoupledIO[T]) extends
  AbstractMonitor[DecoupledIO[T], DecoupledTX[T]](clock, interface) {
//  val txns = Queue[DecoupledTX[T]]()
  var waitCycles = 0
  fork {
    var cycleCount = 0
    var idleCyclesD = 0
    while (true) {
      interface.ready.poke(false.B)
      while (idleCyclesD > 0) {
        idleCyclesD -= 1
        cycleCount += 1
        clock.step()
      }
      interface.ready.poke(1.B)
      if (interface.valid.peek().litToBoolean) {
        val t = DecoupledTX[T](interface.bits.peek(), cycleStamp = cycleCount)
        idleCyclesD = waitCycles
        // For debugging use
        // println("DDUT", interface.bits.peek().litValue(), cycleCount)
        addMonitoredTransaction(t)
      }
      cycleCount += 1
      clock.step()
    }
  }
}
