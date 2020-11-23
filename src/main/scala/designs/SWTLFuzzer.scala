package designs

import verif._
import chisel3._
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tilelink.TLSlaveParameters

import scala.math.pow
import scala.util.Random

// Currently only supports TL-UL, Get and FullPut (A, D Channels, Single beat transactions)
// TODO Support TL-UH
// TODO Need to polish address step size
class SWTLFuzzer (params : TLSlaveParameters, overrideAddr: Option[AddressSet] = None, addrStep : Int = 0x8, randSeed : Int = 1234567890) {
  // Temporary until we have constrained randoms
  val randGen = Random
  randGen.setSeed(randSeed)

  def getRandomLegalAddress (slaves: Seq[AddressSet], addrStep : Int) : Int = {
    val addressRaw = randGen.nextInt(params.maxAddress.toInt + 1) & ~(addrStep - 1)
    val randomAddrSet = slaves(randGen.nextInt(slaves.length))
    val addrSet = overrideAddr.getOrElse(randomAddrSet)
    (addrSet.base | (addrSet.mask & addressRaw)).toInt
  }

  def generateTransactions(numbTxn : Int) : Seq[TLTransaction] = {
    var genTxns = Seq[TLTransaction]()
    var typeTxn = 0

    // TLBundle Params
    var param = 0 // Never changes for A
    var size = 0 // Need to figure out requirements
    var source = 0 // Will remain 0 for now (concurrentTxn = 1)
    var address = 0
    var mask = 0 // Unused, need to figure out mask requirements
    var data = 0

    // Generating Transactions
    for (_ <- 0 until numbTxn) {
      typeTxn = randGen.nextInt

      // Hardcoded since we only have Get and FullPut. TODO add support checking
      // Also hardcoded widths and masks for now (TODO FIX)
      if (typeTxn % 2 == 0) {
        genTxns = genTxns :+ Get(size = 3.U, address.U(64.W), mask = 0xff.U)
      } else {
        // Find a better way to handle address step size
        address = getRandomLegalAddress(params.address, addrStep)
        data = randGen.nextInt(pow(2, 64).toInt) // Will account for transfer sizes
        genTxns = genTxns :+ PutFull(addr = address.U(64.W), mask = 0xff.U, data = data.U(64.W)) // Hardcoded mask for now
      }
    }

    genTxns
  }

  def reset() : Unit = {
    randGen.setSeed(randSeed)
  }
}
