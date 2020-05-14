package verif

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

	// Enqueue has priority over dequeue
	def process (input : QueueIOInTr) : QueueIOOutTr = {
		if (input.validEnq) {
			this.enqueue(input.dataEnq)
		}

		if (input.readyDeq) {
			QueueIOOutTr(this.dequeue)
		} else {
			new QueueIOOutTrNull
		}
	}
}