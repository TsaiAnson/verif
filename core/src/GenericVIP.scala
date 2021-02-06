package verif

import chisel3._
import chiseltest._
import scala.collection.mutable

class GenericDriver[T <: Data] (clock: Clock, interface: T) {
  val inputTransactions: mutable.Queue[T] = mutable.Queue[T]()
  fork {
    while (true) {
      if (inputTransactions.nonEmpty) {
        val t = inputTransactions.dequeue()
        interface.asInstanceOf[Bundle].pokePartial(t.asInstanceOf[Bundle])
      }
      clock.step()
    }
  }
}

class GenericMonitor[T <: Data] (clock: Clock, interface: T) {
  val monitoredTransactions: mutable.Queue[T] = mutable.Queue[T]()
  fork.withRegion(Monitor) {
    while(true) {
      monitoredTransactions += interface.peek()
      clock.step()
    }
  }
}

