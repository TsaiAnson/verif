package verif

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

class AtmProp[T,H,M](proposition: (T, HashMap[String,H], Option[SLMemoryState[M]]) => Boolean, desc: String) extends SequenceElement {
  def check(input: T, hash: HashMap[String, H], ms: Option[SLMemoryState[M]]): Boolean = proposition(input, hash, ms)

  def getProp: (T, HashMap[String,H], Option[SLMemoryState[M]]) => Boolean = proposition

  def &(that: AtmProp[T,H,M]): AtmProp[T,H,M] = new AtmProp[T,H,M]({(t: T, h: HashMap[String, H], m: Option[SLMemoryState[M]])
    => proposition(t, h, m) & that.getProp(t, h, m)}, s"$desc and $that")
  def |(that: AtmProp[T,H,M]): AtmProp[T,H,M] = new AtmProp[T,H,M]({(t: T, h: HashMap[String, H], m: Option[SLMemoryState[M]])
    => proposition(t, h, m) | that.getProp(t, h, m)}, s"$desc or $that")

  // Adding creates another sequence
  def +(that: AtmProp[T,H,M]): Sequence[T,H,M] = new Sequence[T,H,M](this, that)
  def +(that: TimeOp): Sequence[T,H,M] = new Sequence[T,H,M](this, that)
  def +(that: Implies): Sequence[T,H,M] = new Sequence[T,H,M](this, that)
  def +(that: Sequence[T,H,M]): Sequence[T,H,M] = new Sequence(this) + that

  override def toString: String = desc
}

class TimeOp(cycles: Int, cycles1: Int = -1, modifier: Int = 0) extends SequenceElement {
  // Modifier values
  // 0 - exactly
  // 1 - at least
  // 2 - at most (up to, inclusive)
  // 3 - between cycles and cycles1 (inclusive, inclusive)
  assert(modifier >= 0 && modifier <= 3, s"Time operator modifier must be from 0 to 3 (inclusive).")

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

  override def toString: String = {
    if (modifier == 0) {
      s"Exactly $cycles cycles"
    } else if (modifier == 1) {
      s"At least $cycles cycles"
    } else if (modifier == 2) {
      s"Cycles: At most $cycles cycles"
    } else {
      s"Between $cycles to $cycles1 cycles"
    }
  }
}

// Need a better way to group these classes
class Implies extends SequenceElement

// Proposition Set (Packages APs with their time operator)
class PropSet[T,H,M](ap: AtmProp[T,H,M], to: TimeOp, implication: Boolean = false, incomplete: Boolean = false) extends SequenceElement {
  def check(input: T, hash: HashMap[String, H], ms: Option[SLMemoryState[M]], lastPassed: Int, currCycle: Int): Boolean = {
    if (implication) return implication
    // Check TO first (allow short circuit), as AP could have state change (counters) that should only update when TO valid
    to.check(currCycle - lastPassed) && ap.check(input, hash, ms)
  }

  def invalid (currCycle: Int, lastPassedIdx: Int): Boolean = {
    // Assumes one transaction per cycle
    val invalid = to.invalid(currCycle - lastPassedIdx)
    invalid
  }

  def getAP: AtmProp[T,H,M] = ap
  def getTO: TimeOp = to
  def isImplication: Boolean = implication
  def isIncomplete: Boolean = incomplete

  override def toString: String = s"$to $ap"
}
