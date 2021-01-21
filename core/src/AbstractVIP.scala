package verif

import chisel3._

import scala.collection.mutable

// Interface type I, Storage type S (transaction type)
abstract class AbstractDriver[I, S](clock: Clock, interface: I) {
  val inputTransactions: mutable.Queue[S] = mutable.Queue[S]()

  def push(t: S): Unit = {
    inputTransactions += t
  }

  def push(tx:Seq[S]): Unit = {
    for (t <- tx) {
      inputTransactions += t
    }
  }

  def hasNextTransaction: Boolean = {
    inputTransactions.nonEmpty
  }

  def getNextTransaction: S = {
    inputTransactions.dequeue()
  }
}

// Input type I, Storage type S
abstract class AbstractMonitor[I, S](clock: Clock, interface: I) {
  val monitoredTransactions: mutable.Queue[S] = mutable.Queue[S]()

  def getOldestMonitoredTransaction: Option[S] = {
    // Returns oldest T if non-empty, else None
    if (monitoredTransactions.nonEmpty) {
      Some(monitoredTransactions.dequeue())
    } else {
      None
    }
  }

  def getMonitoredTransactions: mutable.MutableList[S] = {
    monitoredTransactions
  }

  def clearMonitoredTransactions(): Unit = {
    monitoredTransactions.clear()
  }

  def addMonitoredTransaction(t: S): Unit = {
    monitoredTransactions += t
  }
}
