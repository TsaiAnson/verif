package designs

import verif._
import chisel3._
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tilelink.TLSlaveParameters

import scala.math.pow
import scala.util.Random
import scala.collection.mutable.ListBuffer

// Currently supports TL-UL, TL-UH (some restrictions in randomization)
class SWTLFuzzer (params : TLSlaveParameters, overrideAddr: Option[AddressSet] = None,
                  burst : Boolean = false, arith : Boolean = false, logic : Boolean = false,
                  hints : Boolean = false, randSeed : Int = 1234567890) {
  // Temporary until we have constrained randoms
  val randGen = Random
  randGen.setSeed(randSeed)

  def getRandomLegalAddress (slaves: Seq[AddressSet]) : Int = {
    val addressRaw = randGen.nextInt(params.maxAddress.toInt + 1)
    val randomAddrSet = slaves(randGen.nextInt(slaves.length))
    val addrSet = overrideAddr.getOrElse(randomAddrSet)
    (addrSet.base | (addrSet.mask & addressRaw)).toInt
  }

  def generateTransactions(numbTxn : Int) : Seq[TLTransaction] = {
    var genTxns = ListBuffer[TLTransaction]()
    var txnCount = 0

    // TLBundle Params
    var typeTxn = 0
    var param = 0
    var size = 0 // Limit from 1 - 6
    var source = 0 // Will remain 0 for now (concurrentTxn = 1)
    var address = 0
    var mask = 0
    var data = 0

    // Generating Transactions
    var redo = false
    while (txnCount < numbTxn - 1) {
      do {
        // Determines transaction type
        typeTxn = randGen.nextInt.abs % 6

        // Need to "re-roll" for unwanted txn types
        redo = false
        if (!arith && typeTxn == 3) {
          redo = true
        } else if (!logic && typeTxn == 4) {
          redo = true
        } else if (!hints && typeTxn == 5) {
          redo = true
        }
      } while (redo)

      // TODO Add support checking for allowed operations
      // Currently supported (TL-UH) Get, PutFull (PutFullBurst), PutPartial (PutPartialBurst),
      // ArithData (ArithDataBurst), LogicData (LogicDataBurst), Intent (Hint)

      if (typeTxn == 0) {
        // Get
        size = randGen.nextInt(6) + 1
        address = getRandomLegalAddress(params.address)
        mask = randGen.nextInt(0xff) + 1 //Max hardcoded at 0xff for now

        genTxns += Get(size = size.U, address.U(64.W), mask = mask.U)

        txnCount += 1
      } else if (typeTxn == 1) {
        // PutFull
        size = randGen.nextInt(6) + 1
        address = getRandomLegalAddress(params.address)
        // Harcoded mask for now
        mask = 0xff

        // TODO Use parameters/beatBytes to determine size bit thresholds
        if (size > 3 && burst) {
          val beatCount = 1 << (size - 3)
          genTxns += PutFullBurst(size = size.U, address.U(64.W), masks = List.fill(beatCount)(0xff.U),
            datas = List.fill(beatCount)(randGen.nextInt(pow(2, 64).toInt).U(64.W)))
        } else {
          data = randGen.nextInt(pow(2, 64).toInt) // Will account for transfer sizes
          genTxns += PutFull(addr = address.U(64.W), mask = mask.U, data = data.U(64.W)) // Hardcoded mask for now
        }

        txnCount += 1
      } else if (typeTxn == 2) {
        // PutPartial
        size = randGen.nextInt(6) + 1
        address = getRandomLegalAddress(params.address)

        if (size > 3 && burst) {
          val beatCount = 1 << (size - 3)
          genTxns += PutFullBurst(size = size.U, address.U(64.W), masks = List.fill(beatCount)((randGen.nextInt(0xff) + 1).U),
            datas = List.fill(beatCount)(randGen.nextInt(pow(2, 64).toInt).U(64.W)))
        } else {
          mask = randGen.nextInt(0xff) + 1
          data = randGen.nextInt(pow(2, 64).toInt) // Will account for transfer sizes
          genTxns += PutFull(addr = address.U(64.W), mask = mask.U, data = data.U(64.W))
        }

        txnCount += 1
      } else if (typeTxn == 3) {
        // ArithData
        param = randGen.nextInt(5)
        size = randGen.nextInt(6) + 1
        address = getRandomLegalAddress(params.address)

        if (size > 3 && burst) {
          val beatCount = 1 << (size - 3)
          genTxns += ArithDataBurst(param = param.U,size = size.U, address.U(64.W), masks = List.fill(beatCount)((randGen.nextInt(0xff) + 1).U),
            datas = List.fill(beatCount)(randGen.nextInt(pow(2, 64).toInt).U(64.W)))
        } else {
          mask = randGen.nextInt(0xff) + 1
          data = randGen.nextInt(pow(2, 64).toInt) // Will account for transfer sizes
          genTxns += ArithData(param = param.U, addr = address.U(64.W), mask = mask.U, data = data.U(64.W)) // Hardcoded mask for now
        }

        txnCount += 1
      } else if (typeTxn == 4) {
        // LogicData
        param = randGen.nextInt(4)
        size = randGen.nextInt(6) + 1
        address = getRandomLegalAddress(params.address)

        if (size > 3) {
          val beatCount = 1 << (size - 3)
          genTxns += LogicDataBurst(param = param.U,size = size.U, address.U(64.W), masks = List.fill(beatCount)((randGen.nextInt(0xff) + 1).U),
            datas = List.fill(beatCount)(randGen.nextInt(pow(2, 64).toInt).U(64.W)))
        } else {
          mask = randGen.nextInt(0xff) + 1
          data = randGen.nextInt(pow(2, 64).toInt) // Will account for transfer sizes
          genTxns += LogicData(param = param.U, addr = address.U(64.W), mask = mask.U, data = data.U(64.W)) // Hardcoded mask for now
        }

        txnCount += 1
      } else {
        // Intent
        param = randGen.nextInt(2)
        size = randGen.nextInt(6) + 1
        address = getRandomLegalAddress(params.address)

        // Unsure how to indicate burst mask...
        // Currently commented out since TLRAMModel doesn't like hints...
//        genTxns += Intent(param = param.U, size = size.U, addr = address.U, mask = 0xff.U)

        if (param == 0) {
          genTxns += Get(size = size.U, address.U(64.W), mask = mask.U)
        } else {
          val fullPartial = randGen.nextInt(2)
          if (size > 3 && burst) {
            val beatCount = 1 << (size - 3)
            if (fullPartial == 0) {
              genTxns += PutFullBurst(size = size.U, address.U(64.W), masks = List.fill(beatCount)(0xff.U),
                datas = List.fill(beatCount)(randGen.nextInt(pow(2, 64).toInt).U(64.W)))
            } else {
              genTxns += PutPartialBurst(size = size.U, address.U(64.W), masks = List.fill(beatCount)((randGen.nextInt(0xff) + 1).U),
                datas = List.fill(beatCount)(randGen.nextInt(pow(2, 64).toInt).U(64.W)))
            }
          } else {
            data = randGen.nextInt(pow(2, 64).toInt) // Will account for transfer sizes
            if (fullPartial == 0) {
              genTxns += PutFull(addr = address.U(64.W), mask = mask.U, data = data.U(64.W)) // Hardcoded mask for now
            } else {
              genTxns += PutPartial(addr = address.U(64.W), mask = mask.U, data = data.U(64.W))
            }
          }
        }

        // Counts for 2 transactions
        txnCount += 2
      }
    }

    genTxns
  }

  def reset() : Unit = {
    randGen.setSeed(randSeed)
  }
}
