package verif

import scala.collection.mutable.{HashMap, ListBuffer}

// Proposition Set (Packages APs with shared time operator)
class PropSet[T,H,M](ap: AtmProp[T,H,M], to: TimeOp, implication: Boolean = false, incomplete: Boolean = false) {
  def check(input: T, hash: HashMap[String, H], ms: Option[SLMemoryState[M]], lastPassedCycle: Int, currCycle: Int): Boolean = {
    if (implication) return true
    // Check TO first (allow short circuit), as AP could have state change (counters) that should only update when TO valid
    to.check(currCycle - lastPassedCycle) && ap.check(input, hash, ms)
  }

  def invalid(lastPassedCycle: Int, currCycle: Int): Boolean = {
    if (implication) return false
    to.invalid(currCycle - lastPassedCycle)
  }

  def getAP: AtmProp[T,H,M] = ap
  def getTO: TimeOp = to
  def isImplication: Boolean = implication
  def isIncomplete: Boolean = incomplete

  override def toString: String = s"PropSet: $to $ap"
}

class Sequence[T,H,M](input: SequenceElement*) {
  val groupedSeq = new ListBuffer[PropSet[T,H,M]]()
  // Index of first implication
  var firstImplication: Int = -1

  // Condensing SequenceElements
  val condensed = new ListBuffer[SequenceElement]
  for (i <- 0 until input.size) {
    // Note: Can only condense AtmProp
    input(i) match {
      case a: AtmProp[T,H,M] =>
        if (condensed.nonEmpty && condensed.last.isInstanceOf[AtmProp[T,H,M]])
          condensed.update(condensed.size-1, condensed.last.asInstanceOf[AtmProp[T,H,M]] & a)
        else condensed += a
      case t: TimeOp =>
        if (i != 0 && input(i-1).isInstanceOf[TimeOp]) println(s"WARNING: Detected 2 TimeOps in a row. Ignoring extra: $t")
        else condensed += t
      case i: Implication =>
        condensed += i
    }
  }

  // Converting to PropSet
  for (i <- condensed.indices) {
    condensed(i) match {
      case a: AtmProp[T,H,M] =>
        if (i == 0) groupedSeq += new PropSet[T,H,M](a, new TimeOp(0, 0))
        else if (condensed(i-1).isInstanceOf[Implication]) groupedSeq += new PropSet[T,H,M](a, new TimeOp(0, 0))
        else groupedSeq += new PropSet[T,H,M](a, condensed(i-1).asInstanceOf[TimeOp])
      case t: TimeOp =>
        // Only add TimeOp if it's the last to add (else the above case will handle)
        if (condensed.size - 1 == i)
          groupedSeq += new PropSet[T,H,M](new AtmProp({(_:T, _: HashMap[String, H], _: Option[SLMemoryState[M]]) => true}, "temp-incomplete propset"), t, incomplete = true)
      case _: Implication =>
        firstImplication = groupedSeq.size
        groupedSeq += new PropSet[T,H,M](new AtmProp[T,H,M]({(_:T, _: HashMap[String, H], _: Option[SLMemoryState[M]]) => true}, "Implication"),
          new TimeOp(0, 0), implication = true)
    }
  }

  def get(index: Int): PropSet[T,H,M] = groupedSeq(index)
  def len: Int = groupedSeq.size
  def set(list: ListBuffer[PropSet[T,H,M]]): Unit = {
    groupedSeq.clear()
    firstImplication = -1
    groupedSeq ++= list
    // Calculating new first Implication
    for ((p, i) <- list.zipWithIndex) if (p.isImplication && firstImplication == -1) firstImplication = i
  }
  def isEmpty: Boolean = groupedSeq.isEmpty

  // Concatenating Sequence
  def +(that: Sequence[T,H,M]): Sequence[T,H,M] = {
    if (that.groupedSeq.isEmpty) {
      return this
    }
    val copyPropSets = new ListBuffer[PropSet[T,H,M]]()
    groupedSeq.copyToBuffer(copyPropSets)
    val newSeq = new Sequence[T,H,M]()
    if (copyPropSets.nonEmpty && copyPropSets.last.isIncomplete) {
      val copyOtherPropSets = new ListBuffer[PropSet[T,H,M]]()
      that.groupedSeq.copyToBuffer(copyOtherPropSets)
      copyOtherPropSets.update(0, new PropSet[T,H,M](copyOtherPropSets.head.getAP, copyPropSets.last.getTO))
      copyPropSets.remove(copyPropSets.size - 1)
      newSeq.set(copyPropSets ++ copyOtherPropSets)
    } else {
      newSeq.set(copyPropSets ++ that.groupedSeq)
    }
    newSeq
  }

  // Static Repetition Operator
  def *(that: Int): Sequence[T,H,M] = {
    var newSeq = new Sequence[T,H,M]()
    for (_ <- 0 until that) {
      newSeq = newSeq + this
    }
    newSeq
  }

  // Adding SequenceElements
  def +(that: SequenceElement): Sequence[T,H,M] = {
    val copyPropSets = new ListBuffer[PropSet[T,H,M]]()
    groupedSeq.copyToBuffer(copyPropSets)
    val newSeq = new Sequence[T,H,M]()
    that match {
      case a: AtmProp[T,H,M] =>
        if (copyPropSets.isEmpty) copyPropSets += new PropSet[T,H,M](a, new TimeOp(0, 0))
        else if (copyPropSets.last.isIncomplete) copyPropSets.update(copyPropSets.size - 1, new PropSet[T,H,M](a, copyPropSets.last.getTO))
        else if (copyPropSets.last.isImplication) copyPropSets += new PropSet[T,H,M](a, new TimeOp(0, 0))
        else copyPropSets.update(copyPropSets.size - 1, new PropSet[T,H,M](copyPropSets.last.getAP & a, copyPropSets.last.getTO))
        newSeq.set(copyPropSets)
      case t: TimeOp =>
        if (copyPropSets.nonEmpty && copyPropSets.last.isIncomplete) println(s"ERROR: Unable to add ($t) to sequence (sequential TimeOps).")
        else copyPropSets += new PropSet[T,H,M](new AtmProp({(_:T, _: HashMap[String, H], _: Option[SLMemoryState[M]]) => true}, "temp-incomplete propset"), t, incomplete = true)
        newSeq.set(copyPropSets)
      case _: Implication =>
        if (copyPropSets.isEmpty) println(s"ERROR: Unable to add Implication to sequence (Initial SequenceElement must be Atomic Proposition).")
        else copyPropSets += new PropSet[T,H,M](new AtmProp[T,H,M]({(_:T, _: HashMap[String, H], _: Option[SLMemoryState[M]]) => true}, "Implication"), new TimeOp(0, 0), implication = true)
        newSeq.set(copyPropSets)
    }
    newSeq
  }

  def printAll(): Unit = groupedSeq.foreach(println)
}