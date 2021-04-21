package verif

import scala.collection.mutable.HashMap

trait SequenceElement

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
  def +(that: Implication): Sequence[T,H,M] = new Sequence[T,H,M](this, that)
  def +(that: Sequence[T,H,M]): Sequence[T,H,M] = new Sequence(this) + that

  override def toString: String = "AtmProp: " + desc
}

class TimeOp(lowerCycles: Int, upperCycles: Int) extends SequenceElement {

  def getLowerCycles: Int = lowerCycles
  def getUpperCycles: Int = upperCycles

  // Returns true if given cycles meets time requirement, false if not (but still can in future)
  def check(elapsedCycles: Int): Boolean = {
    if (elapsedCycles < 0) assert(false, s"ERROR: elapsedCycles should be at least 0. Given: $elapsedCycles")

    elapsedCycles >= lowerCycles && ((upperCycles == -1) || (elapsedCycles <= upperCycles))
  }

  // Returns true if given cycles can never meet time requirement
  def invalid(elapsedCycles: Int): Boolean = {
    (upperCycles != -1) && (elapsedCycles > upperCycles)
  }

  override def toString: String = {
    if (lowerCycles == upperCycles) {
      s"TimeOp: Exactly $lowerCycles cycles"
    } else if (upperCycles == -1) {
      s"TimeOp: At least $lowerCycles cycles"
    } else if (lowerCycles == -1) {
      s"TimeOp: Cycles: At most $upperCycles cycles"
    } else {
      s"TimeOp: Between $lowerCycles to $upperCycles cycles"
    }
  }
}

class Implication extends SequenceElement
