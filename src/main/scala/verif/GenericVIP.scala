package verif

import chisel3._
import chiseltest._
import scala.collection.mutable.{MutableList, Queue}

class GenericDriver[T <: Data] (clock: Clock, interface : T) {
  val inputTransactions = Queue[T]()

  def push(tx:Seq[T]): Unit = {
    for (t <- tx) {
      inputTransactions += t
    }
  }

  fork {
    while (true) {
      if (!inputTransactions.isEmpty) {
        val t = inputTransactions.dequeue
        println("Hi")
        println(t)
        interface.poke(t)
      }
      clock.step()
    }
  }
}

// Using hardcoded for now (ParameterizedCAMAssociative instead of Model)
class GenericMonitor[T <: Data] (clock: Clock, interface: T) {
  val monitoredTransactions = MutableList[T]()

  def getMonitoredTransactions: MutableList[T] = {
    monitoredTransactions
  }

  def clearMonitoredTransactions(): Unit = {
    monitoredTransactions.clear()
  }

  fork.withRegion(Monitor) {
    while(true) {
      monitoredTransactions += interface.peek()
      clock.step()
    }
  }
}

