package verif

import freechips.rocketchip.tilelink._
import verif.TLUtils._
import chisel3._
import chisel3.util.log2Ceil
import freechips.rocketchip.diplomacy.TransferSizes

import scala.collection.mutable
import scala.collection.immutable
import TLTransaction._

import scala.collection.mutable.ListBuffer

// TODO Override operators
class AtmProp[T](proposition: T => Boolean, desc: String) {
  def check(input: T): Boolean = {
    proposition(input)
  }

  override def toString: String = {
    desc
  }
}

// TODO Override operators
class TimeOp(cycles: Int, cycles1: Int = -1, modifier: Int = 0) {
  // Modifier values
  // 0 - exactly
  // 1 - at least
  // 2 - at most (up to, inclusive)
  // 3 - between cycles and cycles1 (inclusive, inclusive)

  def getModifier: Int = {
    modifier
  }

  def getCycles: Int = {
    cycles
  }

  def getCyclesLimit: Int = {
    cycles1
  }

  // Returns true if given cycles meets time requirement, false if not (but still can in future)
  def check(elapsedCycles: Int): Boolean = {
    if (elapsedCycles < 0) {
      assert(false, s"ERROR: curr_cycles should be at least 0. Given: $elapsedCycles")
    }

    if (modifier == 0) {
      elapsedCycles == cycles
    } else if (modifier == 1) {
      elapsedCycles >= cycles
    } else if (modifier == 2) {
      elapsedCycles <= cycles
    } else if (modifier == 3) {
      (elapsedCycles >= cycles) & (elapsedCycles <= cycles1)
    } else {
      assert(false, s"ERROR: Invalid modifier for TimeOp. Given: $modifier")
      false
    }
  }

  // Returns true if given cycles can never meet time requirement (exactly, at most, and between)
  def invalid(elapsedCycles: Int): Boolean = {
    if ((modifier == 0) | (modifier == 2)) {
      elapsedCycles > cycles
    } else if (modifier == 3) {
      elapsedCycles > cycles1
    } else {
      false
    }
  }

  override def toString: String = {
    s"Cycles: $cycles to $cycles1, Modifier: $modifier"
  }
}

// TODO Override operators
class PropSet[T](ap: AtmProp[T], to: TimeOp) {
  def check(input: T, startCycle: Int, currCycle: Int): Boolean = {
    ap.check(input) & to.check(currCycle - startCycle)
  }

  def invalid (startCycle: Int, currCycle: Int, lastPassedIdx: Int): Boolean = {
    val invalid = to.invalid(currCycle - startCycle)
    if (invalid) {
      println(s"ERROR: Atomic proposition '$ap' failed, as it did not meet the TimeOperator requirement ($to). " +
        s"Cycles elapsed: ${currCycle - startCycle}. Index of last passed transaction: $lastPassedIdx.")
    }
    invalid
  }

  def getAP: AtmProp[T] = {
    ap
  }

  def getTO: TimeOp = {
    to
  }

  override def toString: String = {
    s"$to $ap"
  }
}

// Currently only works with alternating types (AtmProp, TimeOp, AtmProp, TimeOp, etc...). TODO Add condensing step
class Sequence[T](input: Any*) {
  val groupedSeq = new ListBuffer[PropSet[T]]()

  for (i <- 0 until input.size) {
    input(i) match {
      case a: AtmProp[T] =>
        assert(i % 2 == 0)
        if (i == 0) {
          if (a.isInstanceOf[AtmProp[T]]) {
            groupedSeq += new PropSet[T](a, new TimeOp(cycles = 0, modifier = 1))
          }
        } else {
          // Cast should not error (Assert in other case would have caught it)
          groupedSeq += new PropSet[T](a, input(i-1).asInstanceOf[TimeOp])
        }
      case t: TimeOp =>
        if (i == 0) {
          assert(false, s"ERROR: First element of sequence must be a AtmProp. Given TimeOp: $t.")
        }
        // Will remove later
        assert(i % 2 == 1)
      case e =>
        assert(false, s"ERROR: Sequence must consist of only AtmProp and TimeOp. Given: ${e.getClass}")
    }
  }
  // Temporary warning
  if (input(input.size - 1).isInstanceOf[TimeOp]) {
    println(s"WARNING: Last element of sequence is TimeOp: ${input(input.size - 1)} and is not matched with an AtmProp.")
  }

  def get(index: Int): PropSet[T] = {
    groupedSeq(index)
  }

  def len: Int = {
    groupedSeq.size
  }

  def printAll(): Unit = {
    groupedSeq.foreach(println)
  }
}

class Property[T](seq: Sequence[T], assertion: Int = 0) {
  // Assertions: 0 - assert, 1 - assume, 2 - cover, 3 - restrict
  // Currently, only assert is implemented

  // We require DecoupledTX for cycleStamps (will change later)
  def check(input: Seq[T]): Boolean = {
    // Keeps track of concurrent instances of properties (SeqIndex, StartCycle)
    // Note: currently does not keep track of intermittent variables
    val concProp = new mutable.ListBuffer[(Int, Int, Int)]()
    var failed_prop = false

    for ((txn, currCycle) <- input.zipWithIndex) {
      // Checking incomplete properties first
      var propMatched = false
      var invalid = false
      if (concProp.nonEmpty) {
        var qIdx = 0
        while (!propMatched && (qIdx < concProp.size)) {
          var (seqIdx, startCycle, lastPassed) = concProp(qIdx)
          var continue = true
          while (continue && (seqIdx < seq.len)) {
            continue = seq.get(seqIdx).check(txn, startCycle, currCycle)
            if (continue) lastPassed = currCycle
            invalid = seq.get(seqIdx).invalid(startCycle, currCycle, lastPassed)
            if (continue) {
              seqIdx += 1
              propMatched = true
            }
          }
          if ((propMatched && seqIdx == seq.len) || invalid) {
            // Property was completed or invalid
            if (invalid) {
              failed_prop = true
            }
            concProp.remove(qIdx)
          } else if (propMatched) {
            concProp.update(qIdx, (seqIdx, startCycle, lastPassed))
          } else {
            qIdx += 1
          }
        }
      }

      if (!propMatched) {
        // If matches first proposition
        val startCycle = currCycle
        if (seq.get(0).check(txn, startCycle, startCycle)) {
          var seqIdx = 1
          var continue = true
          while (continue && (seqIdx < seq.len)) {
            continue = seq.get(seqIdx).check(txn, startCycle, startCycle)
            if (continue) {
              seqIdx += 1
            }
          }
          if (seqIdx < seq.len) {
            // Unfinished, add to queue
            concProp += {(seqIdx, startCycle, startCycle)}
          }
        }
      }
    }
    // Currently does not support dangling txns
    for ((ai, sc, lp) <- concProp) {
      println(s"ERROR: Unresolved property instance. Current atomic proposition: ${seq.get(ai).getAP}. Index of starting transaction: $sc," +
        s" Index of last passed transaction: $lp.")
    }
    concProp.isEmpty && !failed_prop
  }
}

//// Same cycle, same object parameter check
//class TLSelfProperty[T](property: T => Boolean) {
//  def check(input: T): Boolean = {
//    property(input)
//  }
//
//  def check(input: Seq[T]): Seq[Boolean] = {
//    input.map(property(_))
//  }
//}
///*
//Notes: Properties should only return false if the condition is true and property is false (cannot do opcode == Get && \
//  param == 0, as it will fail on non-gets)
// */
//
//case class TLSelfPropertyA(property: TLBundleA => Boolean) extends TLSelfProperty[TLBundleA](property)
//case class TLSelfPropertyD(property: TLBundleD => Boolean) extends TLSelfProperty[TLBundleD](property)
//// Can continue for other TLChannel types BCE, and generic type (TLChannel)
//
//// Future cycles (lax), nonconsecutive
//// Won't need a strict (consecutive) one for TL, but will need for general case
//// TODO Handle repetitions? Overlapping? Init properties that are same as future property (e.g. burst)
//// TODO Add way to have dependent properties (e.g. if size is burst, future property checks if constant fields match)
//class TLFutureProperty[T](init_property: T => Boolean, future_property: T => Boolean, count: Int) {
//  var remaining_count = 0
//  def checkInit(input: T): Unit = {
//    if (init_property(input)) remaining_count += count
//  }
//
//  def checkFuture(input: T): Unit = {
//    if (future_property(input)) remaining_count -= 1
//  }
//
//  // Cannot handle overlapping/concurrent properties (e.g. Get(source = 0) -> Get(source = 1) -> AccessAckData(1) -> AccessAckData(0))
//  // Note: Currently assumes complete transaction sequence
//  def check(input: Seq[T]): Boolean = {
//    for (i <- input) {if (remaining_count == 0) checkInit(i) else checkFuture(i)}
//    remaining_count == 0
//  }
//}
//
//case class TLFuturePropertyA(property1: TLBundleA => Boolean, property2: TLBundleA => Boolean, count: Int = 1) extends
//  TLFutureProperty[TLBundleA](property1, property2, count)
//case class TLFuturePropertyD(property1: TLBundleD => Boolean, property2: TLBundleD => Boolean, count: Int = 1) extends
//  TLFutureProperty[TLBundleD](property1, property2, count)
//case class TLFuturePropertyAD(property1: TLChannel => Boolean, property2: TLChannel => Boolean, count: Int = 1) extends
//  TLFutureProperty[TLChannel](property1, property2, count)
