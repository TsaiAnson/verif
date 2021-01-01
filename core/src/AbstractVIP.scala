package verif

import chisel3._
import chisel3.util._
import chiseltest._
import scala.collection.mutable.{MutableList, Queue}

// Interface type I, Storage type S, dataType D
abstract class AbstractDriver[I, S](clock: Clock, interface: I) {
  val inputTransactions = Queue[S]()

  def push(t: S): Unit = {
    inputTransactions += t
  }

  def push(tx:Seq[S]): Unit = {
    for (t <- tx) {
      inputTransactions += t
    }
  }

  def hasNextTransaction(): Boolean = {
    !inputTransactions.isEmpty
  }

  def getNextTransaction(): S = {
    inputTransactions.dequeue()
  }
}

// Input type I, Storage type S
abstract class AbstractMonitor[I, S](clock: Clock, interface: I) {
  val monitoredTransactions = Queue[S]()

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

  def getOldestMonitoredTransaction: Option[S] = {
    // Returns oldest T if non-empty, else None
    if (!monitoredTransactions.isEmpty) {
      Some(monitoredTransactions.dequeue())
    } else {
      None
    }
  }

  def getMonitoredTransactions: MutableList[S] = {
    monitoredTransactions
  }

  def clearMonitoredTransactions(): Unit = {
    monitoredTransactions.clear()
  }

  def addMonitoredTransaction(t: S): Unit = {
    monitoredTransactions += t
  }
}
