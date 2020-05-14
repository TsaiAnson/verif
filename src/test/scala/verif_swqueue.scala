package verif

import scala.collection.mutable.Queue

class SWIntQueue (length: Int) {
	val internal_queue = Queue[Int]()

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
}