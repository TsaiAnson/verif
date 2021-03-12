package verif

import scala.collection.mutable.{ListBuffer, HashMap}

class Property[T,H,M](seq: Sequence[T,H,M], name: String = "Default Property Name") {
  // Coverage Data, accumulates across transaction streams. Can be cleared with helper method at end.
  var propSetPass = Array.fill[Int](seq.groupedSeq.size)(0) // For each propSet (group of APs), how many times did it Pass
  var propSetFail = Array.fill[Int](seq.groupedSeq.size)(0) // For any failed properties, at which propSet did it fail?
  var txnCoverage = Array[Int]()

  // Warnings
  if (seq.groupedSeq.nonEmpty && seq.firstImplication == -1) println(s"WARNING: Given property sequence does not contain an implication.")

  def check(input: Seq[T], mems: Seq[Option[SLMemoryState[M]]] = Seq()): Boolean = {
    // Cleared before each run
    this.clearCoverage()
    txnCoverage = Array.fill[Int](input.size)(0)

    // Short circuit if seq is empty
    if (seq.isEmpty) return true

    // Keeps track of concurrent instances of properties (SeqIndex, StartCycle, lastPassed)
    // Note: currently does not keep track of intermittent variables
    val concProp = new ListBuffer[(Int, Int, Int)]()
    var failed_prop = false
    // Local Data per concurrent instance
    val concHash = new ListBuffer[HashMap[String, H]]()

    // If no memory states are given, assign all none
    var int_mems = Seq[Option[SLMemoryState[M]]]()
    if (mems.isEmpty) {
      int_mems = Seq.fill(input.length)(None)
    } else {
      int_mems = mems
    }
    assert(int_mems.length == input.length, "Must have a memory state for each transaction")

    for (((txn, ms), currCycle) <- (input zip int_mems).zipWithIndex) {
      // Checking incomplete properties first
      var propMatched = false
      var invalid = false
      if (concProp.nonEmpty) {
        var qIdx = 0
        while (!propMatched && (qIdx < concProp.size)) {
          var (seqIdx, startCycle, lastPassed) = concProp(qIdx)
          val hash = concHash(qIdx)
          var continue = true
          while (continue && (seqIdx < seq.len)) {
            continue = seq.get(seqIdx).check(txn, hash, ms, lastPassed, currCycle)
            invalid = seq.get(seqIdx).invalid(currCycle, lastPassed)
            if (invalid) {
              if (seqIdx >= seq.firstImplication && seq.firstImplication != -1) {
                println(s"ERROR: Implication failed for $this, as atomic proposition '${seq.get(seqIdx).getAP}' did not meet the " +
                  s"TimeOperator requirement (${seq.get(seqIdx).getTO}). Cycles elapsed: ${currCycle - startCycle}. " +
                  s"Index of starting transaction: $startCycle, Index of last passed transaction: $lastPassed.")
              }
            }
            if (continue) {
              lastPassed = currCycle
              propSetPass(seqIdx) += 1
              txnCoverage(currCycle) = 1
              seqIdx += 1
              propMatched = true
            }
          }
          if ((propMatched && seqIdx == seq.len) || invalid) {
            // Property was completed or invalid
            if (invalid) {
              propSetFail(seqIdx) += 1
              failed_prop = true
            }
            concProp.remove(qIdx)
            concHash.remove(qIdx)
          } else if (propMatched) concProp.update(qIdx, (seqIdx, startCycle, lastPassed))
          else qIdx += 1
        }
      }

      if (!propMatched) {
        // If matches first proposition
        val startCycle = currCycle
        val hash = new HashMap[String, H]()
        if (seq.get(0).check(txn, hash, ms, startCycle, startCycle)) {
          propSetPass(0) += 1
          txnCoverage(currCycle) = 1
          var seqIdx = 1
          var continue = true
          while (continue && (seqIdx < seq.len)) {
            continue = seq.get(seqIdx).check(txn, hash, ms, startCycle, startCycle)
            if (continue) {
              propSetPass(seqIdx) += 1
              seqIdx += 1
            }
          }
          // Unfinished, add to queue
          if (seqIdx < seq.len) {
            concProp += {(seqIdx, startCycle, startCycle)}
            concHash += hash
          }
        }
      }
//      println(s"Debug: Curr Cycle $currCycle")
    }
//    println(s"Debug: End of trace, # of incomplete: ${concProp.size}")
    // Currently does not support dangling txns
    val firstImplication = seq.firstImplication
    var incompleteSeq = false
    for ((ai, sc, lp) <- concProp) {
      if (firstImplication != -1 && ai >= firstImplication) {
        propSetFail(ai) += 1
        println(s"ERROR: Unresolved implication within for a property instance ($this). Current atomic proposition: ${seq.get(ai).getAP}. " +
          s"Index of starting transaction: $sc, Index of last passed transaction: $lp.")
        incompleteSeq = true
      }
    }
//    println(s"Debug: incomplete: $incompleteSeq, failed: $failed_prop")
    !incompleteSeq && !failed_prop
  }
  
  def getSequence(): Sequence[T,H,M] = seq

  // Currently just printing out statistics
  // Initiated Properties
  // Completed Properties
  // Failed Properties
  // # of times each PropSet passed
  // # off times each PropSet failed (completely, not just an incorrect match)
  def getCoverage(): Unit = {
    val initCount = propSetPass(seq.firstImplication)
    val completedCount = propSetPass.last
    val failedCount = propSetFail.sum
    var resultString = ""

    resultString += s"\nPROPERTY COVERAGE DATA ($this): \n\n"
    resultString += s"# of Initiated Properties: $initCount \n"
    resultString += s"# of Completed Properties: $completedCount \n"
    resultString += s"# of Failed Properties: $failedCount \n\n"
    resultString += s"Coverage stats of each AP Group: (PASS #, FAIL#)\n"
    for ((propSet, idx) <- seq.groupedSeq.zipWithIndex) {
      if (!propSet.isImplication) {
        resultString += s"${propSet.getAP} : (${propSetPass(idx)}, ${propSetFail(idx)})\n"
      }
    }
    resultString += s"Transaction coverage: (${txnCoverage.sum}/${txnCoverage.length})\n"
    resultString += s"Transaction coverage bitmap: \n ${txnCoverage.mkString("[", ", ", "]")}\n"
    resultString += s"\nEND OF COVERAGE REPORT.\n"
    println(resultString)
  }

  def clearCoverage(): Unit = {
    propSetPass = Array.fill[Int](seq.groupedSeq.size)(0)
    propSetFail = Array.fill[Int](seq.groupedSeq.size)(0)
    txnCoverage = Array[Int]()
  }

  override def toString: String = s"Property $name"
}
