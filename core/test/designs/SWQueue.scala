package verif

import chisel3._

import scala.collection.mutable

class SWQueue[T <: Data](length: Int, gen: T) {
  val internal_queue = mutable.Queue[T]()

  def enqueue (input : T) : Boolean = {
    if (internal_queue.size < length) {
      internal_queue.enqueue(input)
      true
    } else {
      false
    }
  }

  def enqueueSeq (inputs : Seq[T]) : Unit = {
    for (input <- inputs) {
      internal_queue.enqueue(input)
    }
  }

  def dequeue : T = {
    internal_queue.dequeue()
  }

  def dequeueAll : Seq[T] = {
    var outputs = Seq[T]()

    while (internal_queue.nonEmpty) {
      outputs = outputs :+ internal_queue.dequeue()
    }

    outputs
  }

  // The process method must handle the translation between
  // Chisel and Scala-land data types
  def process (input : Seq[DecoupledTX[T]], cycles : Int, waitCycles : Int) : Seq[DecoupledTX[T]] = {
    var enqWaitCycles = 0
    var deqWaitCycles = 0
    var simCycles = cycles
    val txnIterator = input.iterator
    var t : DecoupledTX[T] = null
    val result = new mutable.ListBuffer[DecoupledTX[T]]

    while (simCycles > 0) {
      // Enqueue
      if (t == null && enqWaitCycles == 0 && txnIterator.hasNext) {
        // If previous transaction finished, get next one
        t = txnIterator.next()
        // For debugging use
        // println("NGOLD", t.data.litValue(), cycles - simCycles)

        if (t.waitCycles.litValue().toInt == 0) {
          val success = this.enqueue(t.data)
          if (success) {
            // For debugging use
            // println("EGOLD", t.data.litValue(), cycles - simCycles)
            enqWaitCycles = t.postSendCycles.litValue().toInt
            t = null
          }
        } else {
          // Subtract 1 to count current cycle as a wait cycle
          enqWaitCycles = t.waitCycles.litValue().toInt - 1
        }
      } else if (t != null && enqWaitCycles == 0) {
        // If there is an un-enqueued transaction
        // Enqueue and set postSendCycles
        val success = this.enqueue(t.data)
        if (success) {
          // For debugging use
          // println("EGOLD", t.data.litValue(), cycles - simCycles)
          enqWaitCycles = t.postSendCycles.litValue().toInt
          t = null
        }
      } else if (enqWaitCycles > 0) {
        enqWaitCycles -= 1
      }

      // Dequeue
      if (deqWaitCycles == 0 && this.internal_queue.nonEmpty) {
        val temp = this.dequeue
        val txOut = new DecoupledTX(gen)
        result += txOut.tx(temp, cycles - simCycles)
        // For debugging use
        // println("DGOLD", temp.litValue(), cycles - simCycles)
        deqWaitCycles = waitCycles
      } else if (deqWaitCycles > 0) {
        deqWaitCycles -= 1
      }

      simCycles -= 1
    }

    result
  }
}