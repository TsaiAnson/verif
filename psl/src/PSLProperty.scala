package verif

import chisel3._
import scala.collection.mutable.{ListBuffer, HashMap}

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
          val hash = concHash(qIdx)
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
            concHash.remove(qIdx)
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
//    println(s"Debug: End of trace, # of incomplete: ${concProp.size}")
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
//    println(s"Debug: incomplete: $incompleteSeq, failed: $failed_prop")
    !incompleteSeq && !failed_prop
  }
}
