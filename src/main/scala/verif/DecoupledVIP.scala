package verif

import chisel3._
import chisel3.util._
import chiseltest._
import scala.collection.mutable.{MutableList, Queue}

case class DecoupledTX[T <: Data](data: T, waitCycles: UInt = 0.U, postSendCycles: UInt = 0.U, cycleStamp: Int = 0) extends Transaction

class DecoupledDriver[T <: Data](clock: Clock, interface: DecoupledIO[T]) {
  val inputTransactions = Queue[DecoupledTX[T]]()

  def push(tx:Seq[DecoupledTX[T]]): Unit = {
    for (t <- tx) {
      inputTransactions += t
    }
  }

  fork {
    var cycleCount = 0
    var idleCycles = 0
    while (true) {
      if (!inputTransactions.isEmpty && idleCycles == 0) {
        val t = inputTransactions.dequeue
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
        interface.bits.poke(t.data)
        interface.valid.poke(1.B)
        if (interface.ready.peek().litToBoolean) {
          cycleCount += 1
          clock.step()
          interface.valid.poke(0.B)
          // For debugging use
          // println("EDUT", t.data.litValue(), cycleCount)
          idleCycles = t.postSendCycles.litValue().toInt
        } else {
          while (!interface.ready.peek().litToBoolean) {
            cycleCount += 1
            clock.step()
          }
          interface.valid.poke(0.B)
          // For debugging use
          // println("EDUT", t.data.litValue(), cycleCount)
          idleCycles = t.postSendCycles.litValue().toInt
        }
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

class DecoupledMonitor[T <: Data](clock: Clock, interface: DecoupledIO[T]) {
  val txns = Queue[DecoupledTX[T]]()
  var waitCycles = 0

  def setConfig(variableName: String, newValue : Int) : Unit = {
    var found = false
    for (f <- this.getClass.getDeclaredFields) {
      f.setAccessible(true)
      if (f.getName == variableName) {
        f.set(this, newValue)
        found = true
      }
    }
    if (!found) {
      throw new IllegalArgumentException("Config variable not found.")
    }
  }

  def getMonitoredTransactions: MutableList[DecoupledTX[T]] = {
    txns
  }

  def clearMonitoredTransactions(): Unit = {
    txns.clear()
  }

  fork {
    var cycleCount = 0
    var idleCyclesD = 0
    while (true) {
      interface.ready.poke(0.B)
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
        txns += t
      }
      cycleCount += 1
      clock.step()
    }
  }
}
