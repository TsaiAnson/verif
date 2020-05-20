package verif

import chisel3._

import scala.collection.mutable.{Queue, ListBuffer}

class SWIntQueue (length: Int) {
	val internal_queue = Queue[Int]()

	def enqueue (input : Int) : Boolean = {
		if (internal_queue.size < length) {
			internal_queue.enqueue(input)

			true
		} else {
			false
		}
	}

	def enqueueSeq (inputs : Seq[Int]) : Unit = {
		for (input <- inputs) {
			internal_queue.enqueue(input)
		}
	}

	def dequeue : Int = {
		internal_queue.dequeue()
	}

	def dequeueAll : Seq[Int] = {
		var outputs = Seq[Int]()

		while (internal_queue.nonEmpty) {
			outputs = outputs :+ internal_queue.dequeue()
		}

		outputs
	}

	// The process method must handle the translation between
	// Chisel and Scala-land data types
	def process (input : Seq[DecoupledTX[UInt]], cycles : Int, waitCycles : Int) : Seq[DecoupledTX[UInt]] = {
		var enqWaitCycles = 0
		var deqWaitCycles = 0
		var simCycles = cycles
		val txnIterator = input.iterator
		var t : DecoupledTX[UInt] = null
		val result = new ListBuffer[DecoupledTX[UInt]]

		while (simCycles > 0) {
			// Enqueue
			if (t == null && enqWaitCycles == 0 && txnIterator.hasNext) {
				// If previous transaction finished, get next one
				t = txnIterator.next()

				if (t.waitCycles == 0) {
					val success = this.enqueue(t.data.litValue().toInt)
					if (success) {
						// println("EGOLD", cycles - simCycles)
						enqWaitCycles = t.postSendCycles
						t = null
					}
				} else {
					enqWaitCycles = t.waitCycles
				}
			} else if (t != null && enqWaitCycles == 0) {
				// If there is an un-enqueued transaction
				// Enqueue and set postSendCycles
				val success = this.enqueue(t.data.litValue().toInt)
				if (success) {
					// println("EGOLD", cycles - simCycles)
					enqWaitCycles = t.postSendCycles
					t = null
				}
			} else if (enqWaitCycles > 0) {
				enqWaitCycles -= 1
			}

			// Dequeue
			if (deqWaitCycles == 0 && this.internal_queue.nonEmpty) {
				result += DecoupledTX(this.dequeue.U)
//				println("DGOLD", cycles - simCycles)
				deqWaitCycles = waitCycles
			} else if (deqWaitCycles > 0) {
				deqWaitCycles -= 1
			}

			simCycles -= 1
		}

		result
	}
}