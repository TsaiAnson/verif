package verif

import chisel3._
import chiseltest._
import scala.collection.mutable.{MutableList, Queue}

class GenericDriver[T <: Data] (clock: Clock, interface: T) extends
  AbstractDriver[T, T](clock, interface) {

  fork {
    while (true) {
      if (hasNextTransaction()) {
        val t = getNextTransaction()
        interface.asInstanceOf[Bundle].pokePartial(t.asInstanceOf[Bundle])
      }
      clock.step()
    }
  }
}

class GenericMonitor[T <: Data] (clock: Clock, interface: T) extends
  AbstractMonitor[T, T](clock, interface) {
  fork.withRegion(Monitor) {
    while(true) {
      addMonitoredTransaction(interface.peek())
      clock.step()
    }
  }
}

