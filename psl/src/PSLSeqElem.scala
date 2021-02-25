package verif

import chisel3._
import scala.collection.mutable.HashMap

trait SequenceElement

// Non-data implementation
//class AtmProp[T,  H](proposition: T => Boolean, desc: String) extends SequenceElement {
//  def check(input: T): Boolean = proposition(input)
//
//  def getProp: T=> Boolean = proposition
//
//  def &(that: AtmProp[T,  H]): AtmProp[T,  H] = new AtmProp[T,  H]({t: T => proposition(t) & that.getProp(t)}, s"$desc and $that")
//  def |(that: AtmProp[T,  H]): AtmProp[T,  H] = new AtmProp[T,  H]({t: T => proposition(t) | that.getProp(t)}, s"$desc or $that")
//
//  override def toString: String = desc
//}

class AtmProp[T,H](proposition: (T, HashMap[String,H]) => Boolean, desc: String) extends SequenceElement {
  def check(input: T, hash: HashMap[String, H]): Boolean = proposition(input, hash)

  def getProp: (T, HashMap[String,H]) => Boolean = proposition

  def &(that: AtmProp[T,H]): AtmProp[T,H] = new AtmProp[T,H]({(t: T, h: HashMap[String, H]) => proposition(t, h) & that.getProp(t, h)},
    s"$desc and $that")
  def |(that: AtmProp[T,H]): AtmProp[T,H] = new AtmProp[T,H]({(t: T, h: HashMap[String, H]) => proposition(t, h) | that.getProp(t, h)},
    s"$desc or $that")

  override def toString: String = desc
}

class TimeOp(cycles: Int, cycles1: Int = -1, modifier: Int = 0) extends SequenceElement {
  // Modifier values
  // 0 - exactly
  // 1 - at least
  // 2 - at most (up to, inclusive)
  // 3 - between cycles and cycles1 (inclusive, inclusive)

  def getModifier: Int = modifier
  def getCycles: Int = cycles
  def getCyclesLimit: Int = cycles1

  // Returns true if given cycles meets time requirement, false if not (but still can in future)
  def check(elapsedCycles: Int): Boolean = {
    if (elapsedCycles < 0) assert(false, s"ERROR: curr_cycles should be at least 0. Given: $elapsedCycles")

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

  override def toString: String = s"Cycles: $cycles to $cycles1, Modifier: $modifier"
}

// Need a better way to group these classes
class Implies extends SequenceElement

class PropSet[T,H](ap: AtmProp[T,H], to: TimeOp, implication: Boolean = false, incomplete: Boolean = false) extends SequenceElement {
  def check(input: T, hash: HashMap[String, H], startCycle: Int, currCycle: Int): Boolean = {
    if (implication) return implication
    ap.check(input, hash) & to.check(currCycle - startCycle)
  }

  def invalid (startCycle: Int, currCycle: Int, lastPassedIdx: Int, implicationMet: Boolean): Boolean = {
    // Assumes one transaction per cycle
    val invalid = to.invalid(currCycle - lastPassedIdx)
    if (invalid && implicationMet) {
      println(s"ERROR: Implication failed, as atomic proposition '$ap' did not meet the TimeOperator requirement ($to). " +
        s"Cycles elapsed: ${currCycle - startCycle}. Index of last passed transaction: $lastPassedIdx.")
    }
    invalid
  }

  def getAP: AtmProp[T,H] = ap
  def getTO: TimeOp = to
  def isImplication: Boolean = implication
  def isIncomplete: Boolean = incomplete

  override def toString: String = s"$to $ap"
}
