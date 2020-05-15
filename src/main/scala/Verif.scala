package verif

import org.scalatest._
import chisel3._
import chiseltest._
import chisel3.util._

import collection.mutable.MutableList
import collection.mutable.Queue
import scala.collection.mutable

trait Transaction extends Bundle {
  // Will define later when working with constraint solver
  def rand(): Int = 0


}

// // Playing around with scala
// trait Transaction {
//   def rand(): Unit = {
//    // Gets all fields and sets them to a random int
//    val r = scala.util.Random
//    for (field <- this.getClass.getDeclaredFields) {
//      field.setAccessible(true)
//      // println(field.getName)
//      // println(field.get(this))
//      field.set(this, r.nextInt())
//    }
//   }
// }

// Using hardcoded for now (ParameterizedCAMAssociative instead of Model) etc
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
