package verif

import org.scalatest._
import chisel3._
import chiseltest._
import chisel3.util._

import collection.mutable.MutableList
import collection.mutable.Queue
import scala.collection.mutable

trait Transaction {
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
class GenericDriver[T <: CAMIOInTr] (c : ParameterizedCAMAssociative) {
  val inputTransactions = Queue[CAMIOInTr]()

  def push(tx:Seq[T]): Unit = {
    for (t <- tx) {
      inputTransactions += t
    }
  }

  fork {
    while (true) {
      // Using hardcoded for now
      if (!inputTransactions.isEmpty) {
        val t = inputTransactions.dequeue
        if (!t.isInstanceOf[CAMIOInTrNull]) {
          c.io.en.poke(t.en.B)
          c.io.we.poke(t.we.B)
          c.io.keyRe.poke(t.keyRe.U)
          c.io.keyWr.poke(t.keyWr.U)
          c.io.dataWr.poke(t.dataWr.U)
        }
      }
      c.clock.step()
    }
  }
}

// Using hardcoded for now (ParameterizedCAMAssociative instead of Model)
class GenericMonitor[T <: CAMIOOutTr] (c : ParameterizedCAMAssociative) {
  val monitoredTransactions = MutableList[CAMIOOutTr]()

  def getMonitoredTransactions(): MutableList[CAMIOOutTr] = {
    return monitoredTransactions;
  }

  def clearMonitoredTransactions(): Unit = {
    monitoredTransactions.clear()
  } 

  fork.withRegion(Monitor) {
    while(true) {
      // Hardcoded for now, can work with MACROs later
      monitoredTransactions += CAMIOOutTr(c.io.found.peek.litToBoolean, c.io.dataRe.peek.litValue.toInt)
      c.clock.step()
    }
  }
}

// Wrapper for the Decoupled Driver given in Testers2
class DecoupledDriver[T <: QueueIOInTr] (c: QueueModule[UInt]) {
  def push(tx:Seq[T]): Unit = {
    for (t <- tx) {
      if (!t.isInstanceOf[QueueIOInTrNull]) {
        // TODO: Generalize
        // Should just push bits directly onto the wire
        // Currently uses the decoupled driver
        if (t.validEnq) {
          // The signals would also need to be generalized
          // Use of macros or reflection?
          c.in.enqueueNow(t.dataEnq.U)
        }

        if (t.readyDeq) {
          // There isn't a "regular" dequeue...
          c.out.expectDequeueNow(t.dataDeq.U)
        }
      }
    }
  }

  // TODO: Look into why the below does not work
//  val inputTransactions = Queue[QueueIOInTr]()

//  def push(tx:Seq[T]): Unit = {
//    for (t <- tx) {
//      inputTransactions += t
//    }
//    println(inputTransactions.size)
//  }
//
//  fork {
//    while (true) {
//      // Using hardcoded for now
//      if (!inputTransactions.isEmpty) {
//        println("Hello")
//        val t = inputTransactions.dequeue
//        if (!t.isInstanceOf[QueueIOInTrNull]) {
//          // TODO: Generalize
//          // Should just push bits directly onto the wire
//          // Currently uses the decoupled driver
//          println("What")
//          if (t.validEnq) {
//            // The signals would also need to be generalized
//            // Use of macros or reflection?
//            println("Enq")
//            c.in.enqueueNow(t.dataEnq.U)
//          }
//
//          if (t.readyDeq) {
//            // There isn't a "regular" dequeue...
//            println("Deq")
//            c.out.expectDequeueNow(t.dataDeq.U)
//          }
//        }
//      }
//      c.clock.step()
//    }
//  }
}

class DecoupledMonitor[T <: QueueIOOutTr]( c: QueueModule[UInt]) {
  val monitoredTransactions = MutableList[QueueIOOutTr]()

  def getMonitoredTransactions: MutableList[QueueIOOutTr] = {
    monitoredTransactions;
  }

  def clearMonitoredTransactions(): Unit = {
    monitoredTransactions.clear()
  }

  fork.withRegion(Monitor) {
    while (true) {
      // Need to check that c.out is the correct ReadyValidIO[T]
      if (c.out.valid.peek().litToBoolean && c.out.ready.peek().litToBoolean) {
        monitoredTransactions += QueueIOOutTr(c.out.bits.peek().litValue().toInt)
      }
      c.clock.step()
    }
  }
}
