package verif

import org.scalatest._

import chisel3._
import chiseltest._
import chisel3.util._

import collection.mutable.MutableList
import collection.mutable.Queue

trait Transaction {
	// Will define later when working with constraint solver
  def rand(): Int = 0
}

// // Playing around with scala
// trait Transaction {
//   def rand(): Unit = {
//   	// Gets all fields and sets them to a random int
//   	val r = scala.util.Random
//   	for (field <- this.getClass.getDeclaredFields) {
//   		field.setAccessible(true)
//   		// println(field.getName)
//   		// println(field.get(this))
//   		field.set(this, r.nextInt())
//   	}
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
