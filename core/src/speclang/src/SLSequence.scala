package verif

import scala.collection.mutable.{HashMap, ListBuffer}

// Currently cannot have sequential TimeOps
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
        if (input(i-1).isInstanceOf[TimeOp]) println(s"WARNING: Detected 2 TimeOps in a row. Ignoring second one: $t")
        else condensed += t
      case i: Implies =>
        condensed += i
    }
  }

  // Converting to PropSet
  for (i <- condensed.indices) {
    condensed(i) match {
      case a: AtmProp[T,H,M] =>
        if (i == 0) groupedSeq += new PropSet[T,H,M](a, new TimeOp(cycles = 0, modifier = 1))
        else if (condensed(i-1).isInstanceOf[Implies]) groupedSeq += new PropSet[T,H,M](a, new TimeOp(cycles = 0, modifier = 0))
        else groupedSeq += new PropSet[T,H,M](a, condensed(i-1).asInstanceOf[TimeOp])
      case t: TimeOp =>
        if (i == 0) assert(false, s"ERROR: First element of sequence must be a AtmProp. Given TimeOp: $t.")
      case _: Implies =>
        firstImplication = groupedSeq.size
        if (i == 0) assert(false, s"ERROR: First element of sequence must be a AtmProp. Given Implies.")
        groupedSeq += new PropSet[T,H,M](new AtmProp[T,H,M]({(_:T, _: HashMap[String, H], _: Option[SLMemoryState[M]]) => true}, "Implication"),
          new TimeOp(0), true)
    }
  }
  // Warnings
  if (groupedSeq.nonEmpty && firstImplication == -1) println(s"WARNING: Given sequence does not contain an implication.")
  // Temporary warning
  if (input.nonEmpty && input(input.size - 1).isInstanceOf[TimeOp])
    println(s"WARNING: Last element of sequence is TimeOp: ${input(input.size - 1)} and is not matched with an AtmProp.")

  def get(index: Int): PropSet[T,H,M] = groupedSeq(index)
  def len: Int = groupedSeq.size
  def set(list: ListBuffer[PropSet[T,H,M]]): Unit = {
    groupedSeq.clear()
    firstImplication = -1
    groupedSeq ++= list
    // Calculating new first Implication
    for ((p, i) <- list.zipWithIndex) if (p.isImplication && firstImplication == -1) firstImplication = i
    if (firstImplication == -1) println(s"WARNING: Given sequence does not contain an implication.")
  }
  def isEmpty: Boolean = groupedSeq.isEmpty

  // Concatenating Sequence
  def +(that: Sequence[T,H,M]): Sequence[T,H,M] = {
    val copyPropSets = new ListBuffer[PropSet[T,H,M]]()
    groupedSeq.copyToBuffer(copyPropSets)
    val newSeq = new Sequence[T,H,M]()
    // TODO Unsure of exact behavior, but currently just adding sequences together
    newSeq.set(copyPropSets ++ that.groupedSeq)
    newSeq
  }

  // Adding SequenceElements
  def +(that: SequenceElement): Sequence[T,H,M] = {
    val copyPropSets = new ListBuffer[PropSet[T,H,M]]()
    groupedSeq.copyToBuffer(copyPropSets)
    val newSeq = new Sequence[T,H,M]()
    that match {
      case a: AtmProp[T,H,M] =>
        if (copyPropSets.isEmpty) copyPropSets += new PropSet[T,H,M](a, new TimeOp(cycles = 0, modifier = 1))
        else if (copyPropSets.last.isIncomplete) copyPropSets.update(copyPropSets.size - 1, new PropSet[T,H,M](a, copyPropSets.last.getTO))
        else copyPropSets.update(copyPropSets.size - 1, new PropSet[T,H,M](copyPropSets.last.getAP & a, copyPropSets.last.getTO))
        newSeq.set(copyPropSets)
      case t: TimeOp =>
        if (copyPropSets.isEmpty) println(s"ERROR: Unable to add TimeOp ($t) to sequence (Initial SequenceElement must be Atomic Proposition).")
        else if (copyPropSets.last.isIncomplete) println(s"ERROR: Unable to add TimeOp ($t) to sequence (sequential TimeOps).")
        else copyPropSets += new PropSet[T,H,M](new AtmProp({(_:T, _: HashMap[String, H], _: Option[SLMemoryState[M]]) => true}, "temp-incomplete propset"), t, incomplete = true)
        newSeq.set(copyPropSets)
      case _: Implies =>
        if (copyPropSets.isEmpty) println(s"ERROR: Unable to add Implication to sequence (Initial SequenceElement must be Atomic Proposition).")
        else copyPropSets += new PropSet[T,H,M](new AtmProp[T,H,M]({(_:T, _: HashMap[String, H], _: Option[SLMemoryState[M]]) => true}, "Implication"), new TimeOp(0), true)
        newSeq.set(copyPropSets)
    }
    newSeq
  }

  def printAll(): Unit = groupedSeq.foreach(println)
}