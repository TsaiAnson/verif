package verif

import chisel3._

import scala.collection.mutable.Queue

class SWIntQueue (length: Int) {
	val internal_queue = Queue[Int]()

	def enqueue (input : Int) : Unit = {
		internal_queue.enqueue(input)
	}

	def enqueueSeq (inputs : Seq[Int]) : Unit = {
		for (input <- inputs) {
			if (internal_queue.size < length) {
				internal_queue.enqueue(input)
			}
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
	def process (input : DecoupledTX[UInt]) : DecoupledTX[UInt] = {
		// This SWModel always enqueues and immediately dequeues

		// Enqueue
		this.enqueue(input.data.litValue().toInt)

		// Dequeue
		DecoupledTX(this.dequeue.U)
	}
}