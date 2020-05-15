package verif

import chisel3._
import chisel3.util._
import chiseltest._
import scala.collection.mutable.{MutableList, Queue}

case class DecoupledTX[T <: Data](data: T, waitCycles: Int = 0, postSendCycles: Int = 0) extends Transaction

class DecoupledDriver[T <: Data](clock: Clock, interface: DecoupledIO[T]) {
  val inputTransactions = Queue[DecoupledTX[T]]()

  def push(tx:Seq[DecoupledTX[T]]): Unit = {
    for (t <- tx) {
      inputTransactions += t
    }
  }

  fork {
    while (true) {
      // Using hardcoded for now
      if (!inputTransactions.isEmpty) {
        val t = inputTransactions.dequeue
        interface.bits.poke(t.data)
        interface.valid.poke(1.B)
        if (interface.ready.peek().litToBoolean) {
          clock.step()
          interface.valid.poke(0.B)
        } else {
          while (!interface.ready.peek().litToBoolean) {
            clock.step()
          }
          interface.valid.poke(0.B)
        }
      } else {
        clock.step()
      }
    }
  }
}

class DecoupledMonitor[T <: Data](clock: Clock, interface: DecoupledIO[T]) {
  val txns = Queue[DecoupledTX[T]]()

  def getMonitoredTransactions: MutableList[DecoupledTX[T]] = {
    txns
  }

  def clearMonitoredTransactions(): Unit = {
    txns.clear()
  }

  fork {
    while (true) {
      interface.ready.poke(1.B)
      if (interface.valid.peek().litToBoolean) {
        val t = DecoupledTX[T](interface.bits.peek())
        txns += t
      }
      clock.step()
    }
  }
}
