package designs

import verif._

// Do we want to import chisel?
import chisel3._

import scala.math.pow
import scala.util.Random
//import scala.collection.mutable.Seq

// Currently only supports TL-UL, Get and FullPut (A, D Channels, Single beat transactions)
// TODO Support TL-UH
// Maybe take in slave edge to check for legality?
// Maybe also take in some port params (for max bit-width etc...)
// Fuzzer Parameters hardcoded right now...
class SWTLFuzzer (beatSize : Int = 3, minAddr : Int = 0, maxAddr : Int = 0xff, addrStep : Int = 0x8, randSeed : Int = 1234567890) {
  // Temporary until we have constrained randoms
  val randGen = Random
  randGen.setSeed(randSeed)

  def generateTransactions(numbTxn : Int) : Seq[TLTransaction] = {
    var genTxns = Seq[TLTransaction]()
    var typeTxn = 0
    var param = 0 // Never changes for A
    var size = 0 // 2^n bytes will be written by the slave (for TL-UH, cannot be bigger than
    var source = 0 // Will remain 0 for now (concurrentTxn = 1)
    var address = 0
    var mask = 0
    var data = 0

    // Generating Transactions
    for (_ <- 0 until numbTxn) {
      typeTxn = randGen.nextInt
      size = randGen.nextInt(beatSize + 1) // Ensure size is within one beat for TL-UL
      address = (randGen.nextInt(maxAddr - minAddr + 1) + minAddr) & ~(addrStep - 1) // Must stay within bounds
      mask = randGen.nextInt() & (pow(2,beatSize) - 1).toInt // Need to figure out mask requirements
      data = randGen.nextInt(pow(2,beatSize).toInt)

      // Hardcoded since we only have Get and FullPut
      // Also hardcoded widths
      if (typeTxn % 2 == 0) {
        genTxns = genTxns :+ Get(address.U(64.W))
      } else {
        genTxns = genTxns :+ FullPut(address.U(64.W), data.U(64.W)) // Add mask once requirement is implemented
      }
      // Check if transaction is legal on the TL connection
      // TODO: in SW or in HW (edge)?
    }

    genTxns
  }

  def reset() : Unit = {
    randGen.setSeed(randSeed)
  }
}
