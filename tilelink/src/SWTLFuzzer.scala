package verif

import chisel3._
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tilelink.{TLBundleParameters, TLChannel, TLSlaveParameters}

import scala.math.pow
import scala.util.Random
import scala.collection.mutable.{HashMap, ListBuffer}
import verif.TLTransaction._

// Currently supports TL-UL, TL-UH, TL-C (some restrictions in randomization)
class SWTLFuzzer (params : TLSlaveParameters, bundleParams: TLBundleParameters, overrideAddr: Option[AddressSet] = None, beatSize : Int = 3,
                 // TL-UL
                 get : Boolean = true, putFull : Boolean = true, putPartial : Boolean = true,
                 // TL-UH
                  burst : Boolean = false, arith : Boolean = false, logic : Boolean = false, hints : Boolean = false,
                 // TL-C
                  tlc : Boolean = false, cacheBlockSize : Int = 5, acquire : Boolean = false,
                 // Randomization
                  randSeed : Int = 1234567890) {

  implicit val p: TLBundleParameters = bundleParams

  // Temporary until we have constrained randoms
  val randGen = Random
  randGen.setSeed(randSeed)

  def getRandomLegalAddress (slaves: Seq[AddressSet], size : Int) : Int = {
    val addressRaw = randGen.nextInt(params.maxAddress.toInt + 1) & ~((1 << size) - 1)
    val randomAddrSet = slaves(randGen.nextInt(slaves.length))
    val addrSet = overrideAddr.getOrElse(randomAddrSet)
    (addrSet.base | (addrSet.mask & addressRaw)).toInt
  }

  def generateTransactions(numbTxn : Int) : Seq[TLChannel] = {
    var genTxns = ListBuffer[TLChannel]()

    // TLBundle Params
    var typeTxn = 0
    var param = 0
    var size = 0 // Limit from 1 - 5 TODO: Use max transfersize to determine
    var source = 0 // Will remain 0 for now (concurrentTxn = 1)
    var address = 0
    var mask = 0
    var data = 0

    // Internal state for Acquire/Release addresses (make sure releasing valid addresses)
    // Permissions: 0 - None, 1 - Read (Branch), 2 - Read/Write (Tip)
    // NOTE: This is a close approximation of the internal state, as this model does not take into account of
    // cache size, random evictions, etc. Some of the transactions will be invalid when model is run.
    // The New Driver Master VIP can reject/ignore invalid transactions
    val intState = HashMap[Int,Int]()

    // Generating Transactions
    var txnBound = 6
    if (tlc) {
      txnBound = 8
    }

    var redo = false
    while (genTxns.length < numbTxn) {
      do {
        // Determines transaction type
        // 0 - Get
        // 1 - PutFull
        // 2 - PutPartial
        // 3 - Arithmetic
        // 4 - Logistic
        // 5 - Hints
        // 6 - Acquire
        // 7 - Release

        typeTxn = randGen.nextInt.abs % txnBound

        // Need to "re-roll" for unwanted txn types
        redo = false
        if (!get && typeTxn == 0) {
          redo = true
        } else if (!putFull && typeTxn == 1) {
          redo = true
        } else if (!putPartial && typeTxn == 2) {
          redo = true
        } else if (!arith && typeTxn == 3) {
          redo = true
        } else if (!logic && typeTxn == 4) {
          redo = true
        } else if (!hints && typeTxn == 5) {
          redo = true
        } else if (!acquire && typeTxn == 6) {
          redo = true
        } else if (!acquire && typeTxn == 7) {
          redo = true
        }
      } while (redo)

      // TODO Add support checking for allowed operations
      // Currently supported (TL-UH) Get, PutFull (PutFullBurst), PutPartial (PutPartialBurst),
      // ArithData (ArithDataBurst), LogicData (LogicDataBurst), Intent (Hint)
      // Currently supported (TL-C) AcquireBlock, AcquirePerm, Release, ReleaseData (ReleaseDataBurst)

      if (typeTxn == 0) {
        // Get
        size = randGen.nextInt(5) + 1
        address = getRandomLegalAddress(params.address, size)
        mask = (1 << (1 << beatSize)) - 1

        genTxns += Get(address, size, mask, source)

      } else if (typeTxn == 1) {
        // PutFull
        size = randGen.nextInt(5) + 1
        address = getRandomLegalAddress(params.address, size)
        if (size > beatSize) {
          mask = (1 << (1 << beatSize)) - 1
        } else {
          mask = (1 << (1 << size)) - 1
        }

        if (size > beatSize && burst) {
          val beatCount = 1 << (size - beatSize)
          // TODO: wtf
          val data = List.fill(beatCount)(randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt)).map(BigInt(_))
          val masks = List.fill(beatCount)(mask)
          genTxns ++= PutBurst(address, data, masks, source)
        } else {
          val data = randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt) // Will account for transfer sizes
          genTxns += Put(address, data, mask)
        }

      } else if (typeTxn == 2) {
        // PutPartial
        size = randGen.nextInt(5) + 1
        address = getRandomLegalAddress(params.address, size)
        mask = randGen.nextInt((1 << (1 << beatSize)))

        if (size > beatSize && burst) {
          val beatCount = 1 << (size - beatSize)
          val data = List.fill(beatCount)(randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt)).map(BigInt(_))
          val masks = List.fill(beatCount)((randGen.nextInt(mask) + 1))
          genTxns ++= PutBurst(address, data, masks, source)
        } else {
          data = randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt) // Will account for transfer sizes
          genTxns += Put(address, data, mask, size, source)
        }

      } else if (typeTxn == 3) {
        // ArithData
        param = randGen.nextInt(5)
        size = randGen.nextInt(5) + 1
        if (tlc) {
          address = intState.keys.toList(randGen.nextInt(intState.keys.size))
        } else{
          address = getRandomLegalAddress(params.address, size)
        }

        if (size > beatSize) {
          mask = (1 << (1 << beatSize)) - 1
        } else {
          mask = (1 << (1 << size)) - 1
        }

        if (size > beatSize && burst) {
          val beatCount = 1 << (size - beatSize)
          val data = List.fill(beatCount)(randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt)).map(BigInt(_))
          genTxns ++= ArithBurst(param, address, data, source)
        } else {
          val data = randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt) // Will account for transfer sizes
          genTxns += Arith(param, address, BigInt(data), mask, size, source)
        }

      } else if (typeTxn == 4) {
        // LogicData
        param = randGen.nextInt(4)
        size = randGen.nextInt(5) + 1
        if (tlc) {
          address = intState.keys.toList(randGen.nextInt(intState.keys.size))
        } else{
          address = getRandomLegalAddress(params.address, size)
        }

        if (size > beatSize) {
          mask = (1 << (1 << beatSize)) - 1
        } else {
          mask = (1 << (1 << size)) - 1
        }

        if (size > beatSize) {
          val beatCount = 1 << (size - beatSize)
          val data = List.fill(beatCount)(randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt)).map(BigInt(_))
          genTxns += LogicBurst(param, address, data, source)
        } else {
          data = randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt) // Will account for transfer sizes
          genTxns += Logic(param, address, data, source)
        }

      } else if (typeTxn == 5) {
        // Intent
        param = randGen.nextInt(2)
        size = randGen.nextInt(5) + 1
        address = getRandomLegalAddress(params.address, size)
        if (size > beatSize) {
          mask = (1 << (1 << beatSize)) - 1
        } else {
          mask = (1 << (1 << size)) - 1
        }

        // Unsure how to indicate burst mask...
        genTxns += Intent(param, address, size, source)

        if (param == 0) {
          genTxns += Get(address, size, mask, source)
        } else {
          val fullPartial = randGen.nextInt(2)
          if (size > beatSize && burst) {
            val beatCount = 1 << (size - beatSize)
            val data = List.fill(beatCount)(randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt))
            genTxns += PutBurst(address, data, mask, source)
          } else {
            val data = randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt) // Will account for transfer sizes
            genTxns += Put(address, data, mask, source)
          }
        }

      } else if (typeTxn == 6) {
        // Acquire
        size = cacheBlockSize
        mask = (1 << (1 << beatSize)) - 1

        var redoAcquire = true
        var repeats = 0
        while (redoAcquire && repeats < 10) {
          redoAcquire = false

          address = getRandomLegalAddress(params.address, cacheBlockSize)

          // If given address is already acquired, try increasing permissions or find new address. After 10 tries, convert
          // to release
          if (intState.contains(address)) {
            if (intState(address) == 0) {
              // Permissions can be from NtoB or NtoT
              param = randGen.nextInt(2)
            } else if (intState(address) == 1) {
              // Permissions from B to T
              param = 2
            } else {
              redoAcquire = true
              repeats += 1
            }
          } else {
            // Permissions can be from NtoB or NtoT
            param = randGen.nextInt(2)
          }
        }

        if (repeats != 10) {
          if (randGen.nextInt() % 2 == 1) {
            genTxns += AcquireBlock(param, address, mask, size, source)
          } else {
            genTxns += AcquirePerm(param, address, mask, size, source)
          }

          intState(address) = if (param == 0) 1 else 2
        } else {
          // Converting to releaseData (since old permissions must be 2 for redo)
          param = randGen.nextInt(2) // TtoB or TtoN
          val data = List.fill(1 << (cacheBlockSize - beatSize))(randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt))
          genTxns += ReleaseDataBurst(param, address, data, source)
        }

      } else if (typeTxn == 7) {
        // Release
        size = cacheBlockSize
        mask = (1 << (1 << beatSize)) - 1

        var redoRelease = true
        var repeats = 0
        while (redoRelease && repeats < 10) {
          redoRelease = false

          if (intState.isEmpty) {
            // If nothing acquired yet, skip directly to acquire with random address
            repeats = 10
            address = getRandomLegalAddress(params.address, cacheBlockSize)
            // Getting block-aligned address
            address = address & ~((1 << cacheBlockSize) - 1)
          } else {
            address = intState.keys.toList(randGen.nextInt(intState.keys.size))

            if (intState(address) == 0) {
              redoRelease = true
              repeats += 1
            } else if (intState(address) == 2) {
              // TtoB or TtoN
              param = randGen.nextInt(2)
            } else {
              // BtoN
              param = 2
            }
          }
        }

        // If no valid address to release, do acquire instead so that next time there will be a release
        if (repeats != 10) {
          if (param < 2) {
            val data = List.fill(1 << (cacheBlockSize - beatSize))(randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt))
            genTxns ++= ReleaseDataBurst(param, address, data, source)
          } else {
            genTxns += Release(param, address, size, source)
          }

          intState(address) = if (param == 0) 1 else 0
        } else {
          // Converting to Acquire
          param = randGen.nextInt(2) // NtoB or NtoT
          if (randGen.nextInt() % 2 == 1) {
            genTxns += AcquireBlock(param, address, mask, size, source)
          } else {
            genTxns += AcquirePerm(param, address, mask, size, source)
          }

          intState(address) = if (param == 0) 1 else 2
        }

      }
    }

    genTxns
  }

  def reset() : Unit = {
    randGen.setSeed(randSeed)
  }
}
