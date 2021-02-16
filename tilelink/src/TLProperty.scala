package verif

import chisel3._
import scala.collection.mutable.{ListBuffer, HashMap}

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

// Currently cannot have sequential TimeOps
class Sequence[T,H](input: SequenceElement*) {
  val groupedSeq = new ListBuffer[PropSet[T,H]]()
  // Index of first implication
  var firstImplication: Int = -1

  // Condensing SequenceElements
  val condensed = new ListBuffer[SequenceElement]
  for (i <- 0 until input.size) {
    // Note: Can only condense AtmProp
    input(i) match {
      case a: AtmProp[T,H] =>
        if (condensed.nonEmpty && condensed.last.isInstanceOf[AtmProp[T,H]])
          condensed.update(condensed.size-1, condensed.last.asInstanceOf[AtmProp[T,H]] & a)
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
      case a: AtmProp[T,H] =>
        if (i == 0) groupedSeq += new PropSet[T,H](a, new TimeOp(cycles = 0, modifier = 1))
        else if (condensed(i-1).isInstanceOf[Implies]) groupedSeq += new PropSet[T,H](a, new TimeOp(cycles = 0, modifier = 0))
        else groupedSeq += new PropSet[T,H](a, condensed(i-1).asInstanceOf[TimeOp])
      case t: TimeOp =>
        if (i == 0) assert(false, s"ERROR: First element of sequence must be a AtmProp. Given TimeOp: $t.")
      case _: Implies =>
        firstImplication = groupedSeq.size
        if (i == 0) assert(false, s"ERROR: First element of sequence must be a AtmProp. Given Implies.")
        groupedSeq += new PropSet[T,H](new AtmProp[T,H]({(_:T, _: HashMap[String, H]) => true}, "Implication"),
          new TimeOp(0), true)
    }
  }
  // Warnings
  if (groupedSeq.nonEmpty && firstImplication == -1) println(s"WARNING: Given sequence does not contain an implication.")
  // Temporary warning
  if (input.nonEmpty && input(input.size - 1).isInstanceOf[TimeOp])
    println(s"WARNING: Last element of sequence is TimeOp: ${input(input.size - 1)} and is not matched with an AtmProp.")

  def get(index: Int): PropSet[T,H] = groupedSeq(index)
  def len: Int = groupedSeq.size
  def set(list: ListBuffer[PropSet[T,H]]): Unit = {
    groupedSeq.clear()
    firstImplication = -1
    groupedSeq ++= list
    // Calculating new first Implication
    for ((p, i) <- list.zipWithIndex) if (p.isImplication && firstImplication == -1) firstImplication = i
    if (firstImplication == -1) println(s"WARNING: Given sequence does not contain an implication.")
  }
  def isEmpty: Boolean = groupedSeq.isEmpty

  // Concatenating Sequence
  def +(that: Sequence[T,H]): Sequence[T,H] = {
    val copyPropSets = new ListBuffer[PropSet[T,H]]()
    groupedSeq.copyToBuffer(copyPropSets)
    val newSeq = new Sequence[T,H]()
    // TODO Unsure of exact behavior, but currently just adding sequences together
    newSeq.set(copyPropSets ++ that.groupedSeq)
    newSeq
  }

  // Adding SequenceElements
  def +(that: SequenceElement): Sequence[T,H] = {
    val copyPropSets = new ListBuffer[PropSet[T,H]]()
    groupedSeq.copyToBuffer(copyPropSets)
    val newSeq = new Sequence[T,H]()
    that match {
      case a: AtmProp[T,H] =>
        if (copyPropSets.isEmpty) copyPropSets += new PropSet[T,H](a, new TimeOp(cycles = 0, modifier = 1))
        else if (copyPropSets.last.isIncomplete) copyPropSets.update(copyPropSets.size - 1, new PropSet[T,H](a, copyPropSets.last.getTO))
        else copyPropSets.update(copyPropSets.size - 1, new PropSet[T,  H](copyPropSets.last.getAP & a, copyPropSets.last.getTO))
        newSeq.set(copyPropSets)
      case t: TimeOp =>
        if (copyPropSets.isEmpty) println(s"ERROR: Unable to add TimeOp ($t) to sequence (Initial SequenceElement must be Atomic Proposition).")
        else if (copyPropSets.last.isIncomplete) println(s"ERROR: Unable to add TimeOp ($t) to sequence (sequential TimeOps).")
        else copyPropSets += new PropSet[T,  H](new AtmProp({(_:T, _: HashMap[String, H]) => true}, "temp-incomplete propset"), t, incomplete = true)
        newSeq.set(copyPropSets)
      case _: Implies =>
        if (copyPropSets.isEmpty) println(s"ERROR: Unable to add Implication to sequence (Initial SequenceElement must be Atomic Proposition).")
        else copyPropSets += new PropSet[T,  H](new AtmProp[T,  H]({(_:T, _: HashMap[String, H]) => true}, "Implication"), new TimeOp(0), true)
        newSeq.set(copyPropSets)
    }
    newSeq
  }

  def printAll(): Unit = groupedSeq.foreach(println)
}

class Property[T,  H](seq: Sequence[T,  H], assertion: Int = 0) {
  // Assertions: 0 - assert, 1 - assume, 2 - cover, 3 - restrict
  // Currently, only assert is implemented

  def check(input: Seq[T]): Boolean = {
    // Short circuit if seq is empty
    if (seq.isEmpty) return true

    // Keeps track of concurrent instances of properties (SeqIndex, StartCycle, lastPassed)
    // Note: currently does not keep track of intermittent variables
    val concProp = new ListBuffer[(Int, Int, Int)]()
    var failed_prop = false
    // Local Data per concurrent instance
    val concHash = new ListBuffer[HashMap[String, H]]()

    for ((txn, currCycle) <- input.zipWithIndex) {
      // Checking incomplete properties first
      var propMatched = false
      var invalid = false
      if (concProp.nonEmpty) {
        var qIdx = 0
        while (!propMatched && (qIdx < concProp.size)) {
          var (seqIdx, startCycle, lastPassed) = concProp(qIdx)
          var hash = concHash(qIdx)
          var continue = true
          while (continue && (seqIdx < seq.len)) {
            continue = seq.get(seqIdx).check(txn, hash, startCycle, currCycle)
            if (continue) lastPassed = currCycle
            invalid = seq.get(seqIdx).invalid(startCycle, currCycle, lastPassed, (seqIdx >= seq.firstImplication && seq.firstImplication != -1))
            if (continue) {
              seqIdx += 1
              propMatched = true
            }
          }
          if ((propMatched && seqIdx == seq.len) || invalid) {
            // Property was completed or invalid
            if (invalid) failed_prop = true
            concProp.remove(qIdx)
          } else if (propMatched) concProp.update(qIdx, (seqIdx, startCycle, lastPassed))
          else qIdx += 1
        }
      }

      if (!propMatched) {
        // If matches first proposition
        val startCycle = currCycle
        val hash = new HashMap[String, H]()
        if (seq.get(0).check(txn, hash, startCycle, startCycle)) {
          var seqIdx = 1
          var continue = true
          while (continue && (seqIdx < seq.len)) {
            continue = seq.get(seqIdx).check(txn, hash, startCycle, startCycle)
            if (continue) seqIdx += 1
          }
          // Unfinished, add to queue
          if (seqIdx < seq.len) {
            concProp += {(seqIdx, startCycle, startCycle)}
            concHash += hash
          }
        }
      }
    }
    // Currently does not support dangling txns
    val firstImplication = seq.firstImplication
    var incompleteSeq = false
    for ((ai, sc, lp) <- concProp) {
      if (firstImplication != -1 && ai >= firstImplication) {
        println(s"ERROR: Unresolved implication within a property instance. Current atomic proposition: ${seq.get(ai).getAP}. " +
          s"Index of starting transaction: $sc, Index of last passed transaction: $lp.")
        incompleteSeq = true
      }
    }
    !incompleteSeq && !failed_prop
  }
}

package object PSL {
  // Quick TimeOperation
  def ###(cycles: Int): TimeOp = ###(cycles, cycles)

  def ###(start: Int, end: Int): TimeOp = {
    if (start == -1) new TimeOp(end, modifier = 2) // At most
    else if (end == -1) new TimeOp(start, modifier = 1) // At least
    else if (start == end) new TimeOp(start, modifier = 0) // Exactly
    else new TimeOp(start, end, 3) // Between
  }

  // Quick Implication
  def Implies: Implies = new Implies

  // Quick Atomic Property
  def qAP[T,  H](proposition: (T, HashMap[String, H]) => Boolean, desc: String): AtmProp[T,  H] = new AtmProp[T,  H](proposition, desc)

  // Quick Property
  def qProp[T,  H](input: SequenceElement*): Property[T,  H] = new Property[T,  H](new Sequence[T,  H](input:_*))
}
