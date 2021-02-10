package verif

import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tilelink.{TLBundleParameters, TLChannel, TLSlaveParameters}

import scala.math.pow
import scala.util.Random
import scala.collection.mutable.{HashMap, ListBuffer}
import verif.TLTransaction._
import verif.TLUtils.RWPermState

// Currently supports TL-UL, TL-UH, TL-C (some restrictions in randomization)
class TLTransactionGenerator ( params: TLSlaveParameters, bundleParams: TLBundleParameters, overrideAddr: Option[AddressSet] = None,
                               beatSize : Int = 3,
                               // TL-UL
                               get : Boolean = true, putFull : Boolean = true, putPartial : Boolean = true,
                               // TL-UH
                               burst : Boolean = false, arith : Boolean = false, logic : Boolean = false, hints : Boolean = false,
                               // TL-C
                               tlc : Boolean = false, cacheBlockSize : Int = -1, acquire : Boolean = false,
                               // Randomization
                               randSeed : Int = 1234567890) {
  assert(!tlc || (tlc && cacheBlockSize != 1), "TLTransactionGenerator: Please set CACHEBLOCKSIZE if TLC is enabled")

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

  // permState is used to ensure generated TL-C transactions are legal ()
  def generateTransactions(numbTxn : Int, permState: RWPermState = new RWPermState()) : Seq[TLChannel] = {
    // Results
    var genTxns = ListBuffer[TLChannel]()

    // Internal state to ensure we don't duplicate releases within one generate (acquire is OK)
    var releasedAddr = ListBuffer[Int]()

    // TLBundle Params
    var typeTxn = 0
    var param = 0
    var size = 0 // TODO: Use max transfersize to determine
    var source = 0 // Will remain 0 for now (concurrentTxn = 1)
    var address = 0
    var mask = 0
    var data = 0

    // Enable generating TLC messages if set
    var txnBound = 6
    if (tlc) {
      txnBound = 8
    }

    var redo = false
    var count = 0
    releasedAddr.clear()
    while (count < numbTxn) {
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
        if ((!get || !params.supportsGet) && typeTxn == 0) {
          redo = true
        } else if ((!putFull || !params.supportsPutFull ) && typeTxn == 1) {
          redo = true
        } else if ((!putPartial || !params.supportsPutPartial) && typeTxn == 2) {
          redo = true
        } else if ((!arith || !params.supportsArithmetic) && typeTxn == 3) {
          redo = true
        } else if ((!logic || !params.supportsLogical) && typeTxn == 4) {
          redo = true
        } else if ((!hints || !params.supportsHint) && typeTxn == 5) {
          redo = true
        } else if ((!acquire || !params.supportsAcquireB) && typeTxn == 6) {
          redo = true
        } else if ((!acquire || !params.supportsAcquireB) && typeTxn == 7) {
          // Shouldn't be able to release if cannot acquire
          redo = true
        }
      } while (redo)

      if (typeTxn == 0) {
        count += 1
        // Get
        size = randGen.nextInt(5) + 1
        address = getRandomLegalAddress(params.address, size)
        if (size > beatSize) {
          mask = (1 << (1 << beatSize)) - 1
        } else {
          mask = (1 << (1 << size)) - 1
        }

        genTxns += Get(address, size, mask, source)

      } else if (typeTxn == 1) {
        count += 1
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
          val data = List.fill(beatCount)(randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt)).map(BigInt(_))
          val masks = List.fill(beatCount)(mask)
          genTxns ++= PutBurst(address, data, masks, source)
        } else {
          val data = randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt) // Will account for transfer sizes
          genTxns += Put(address, data, mask, size, source, false)
        }

      } else if (typeTxn == 2) {
        count += 1
        // PutPartial
        size = randGen.nextInt(5) + 1
        address = getRandomLegalAddress(params.address, size)

        if (size > beatSize) {
          mask = randGen.nextInt(1 << (1 << beatSize))
        } else {
          mask = randGen.nextInt(1 << (1 << size))
        }

        if (size > beatSize && burst) {
          val beatCount = 1 << (size - beatSize)
          val data = List.fill(beatCount)(randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt)).map(BigInt(_))
          val masks = List.fill(beatCount)(randGen.nextInt(1 << (1 << beatSize)))
          genTxns ++= PutBurst(address, data, masks, source)
        } else {
          data = randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt) // Will account for transfer sizes
          genTxns += Put(address, data, mask, size, source, true)
        }

      } else if (typeTxn == 3) {
        count += 1
        // ArithData
        param = randGen.nextInt(5)
        size = randGen.nextInt(5) + 1
        if (tlc) {
          address = permState.getAllAddr(randGen.nextInt(permState.getAllAddr.size))
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
          genTxns ++= ArithBurst(TLArithParam.fromInt(param), address, data, source)
        } else {
          val data = randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt) // Will account for transfer sizes
          genTxns += Arith(TLArithParam.fromInt(param), address, BigInt(data), mask, size, source)
        }

      } else if (typeTxn == 4) {
        count += 1
        // LogicData
        param = randGen.nextInt(4)
        size = randGen.nextInt(5) + 1
        if (tlc) {
          address = permState.getAllAddr(randGen.nextInt(permState.getAllAddr.size))
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
          genTxns ++= LogicBurst(TLLogicParam.fromInt(param), address, data, source)
        } else {
          data = randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt) // Will account for transfer sizes
          genTxns += Logic(TLLogicParam.fromInt(param), address, data, mask, size, source)
        }

      } else if (typeTxn == 5) {
        count += 1
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
        genTxns += Intent(TLIntentParam.PrefetchRead, address, size, source)

        if (param == 0) {
          genTxns += Get(address, size, mask, source)
        } else {
          val fullPartial = randGen.nextInt(2)
          if (size > beatSize && burst) {
            val beatCount = 1 << (size - beatSize)
            val data = List.fill(beatCount)(randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt)).map(BigInt(_))
            if (fullPartial == 0) {
              genTxns ++= PutBurst(address, data, List.fill(beatCount)(mask), source)
            } else {
              genTxns ++= PutBurst(address, data, List.fill(beatCount)(randGen.nextInt(1 << (1 << beatSize))), source)
            }

          } else {
            val data = randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt) // Will account for transfer sizes
            if (fullPartial == 0) {
              genTxns += Put(address, BigInt(data), mask, size, source, false)
            } else {
              mask = randGen.nextInt(1 << (1 << size))
              genTxns += Put(address, BigInt(data), mask, size, source, true)
            }

          }
        }

      } else if (typeTxn == 6) {
        count += 1
        // Acquire
        size = cacheBlockSize
        mask = (1 << (1 << beatSize)) - 1

        var redoAcquire = true
        var repeats = 0
        while (redoAcquire && repeats < 10) {
          redoAcquire = false

          address = getRandomLegalAddress(params.address, cacheBlockSize)

          // Try increasing permissions or find new address. After 10 failed attempts, convert to release
          if (permState.getPerm(address) == 2) {
            redoAcquire = true
            repeats += 1
          } else if (permState.getPerm(address) == 1) {
            // Must be BtoT for Acquire
            param = 2
          } else {
            // Permissions can be from NtoB or NtoT
            param = randGen.nextInt(2)
          }
        }

        if (repeats != 10) {
          if (randGen.nextInt() % 2 == 1) {
            genTxns += AcquireBlock(TLPermission.Grow.NtoB, address, mask, size, source)
          } else {
            genTxns += AcquirePerm(TLPermission.Grow.NtoB, address, mask, size, source)
          }
        } else {
          // Converting to releaseData (since old permissions must be 2 for redo)
          param = randGen.nextInt(2) // TtoB or TtoN

          val cacheMask = ~((1 << cacheBlockSize) - 1)
          while (releasedAddr.contains(address & cacheMask)) {
            address = permState.getAllAddr(randGen.nextInt(permState.getAllAddr.size))
          }
          val data = List.fill(1 << (cacheBlockSize - beatSize))(randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt)).map(BigInt(_))
          genTxns ++= ReleaseDataBurst(TLPermission.PruneOrReport.TtoB, address, data, source)
        }

      } else if (typeTxn == 7) {
        count += 1
        // Release
        size = cacheBlockSize
        mask = (1 << (1 << beatSize)) - 1
        val cacheMask = ~((1 << cacheBlockSize) - 1)

        var redoRelease = true
        var repeats = 0
        while (redoRelease && repeats < 10) {
          redoRelease = false

          if (permState.getAllAddr.isEmpty) {
            // If nothing acquired yet, skip directly to acquire with random address
            repeats = 10
            address = getRandomLegalAddress(params.address, cacheBlockSize)
            // Getting block-aligned address
            address = address & ~((1 << cacheBlockSize) - 1)
          } else {
            address = permState.getAllAddr(randGen.nextInt(permState.getAllAddr.size))

            if (permState.getPerm(address) == 0 || releasedAddr.contains(address & cacheMask)) {
              redoRelease = true
              repeats += 1
            } else if (permState.getPerm(address) == 2) {
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
            val data = List.fill(1 << (cacheBlockSize - beatSize))(randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt)).map(BigInt(_))
            genTxns ++= ReleaseDataBurst(TLPermission.PruneOrReport.TtoB, address, data, source)
          } else {
            genTxns += Release(TLPermission.PruneOrReport.NtoN, address, size, source)
          }

          releasedAddr += address
        } else {
          // Converting to Acquire
          param = randGen.nextInt(2) // NtoB or NtoT
          if (randGen.nextInt() % 2 == 1) {
            genTxns += AcquireBlock(TLPermission.Grow.NtoB, address, mask, size, source)
          } else {
            genTxns += AcquirePerm(TLPermission.Grow.NtoT, address, mask, size, source)
          }
        }

      }
    }

    genTxns
  }

  def reset() : Unit = {
    randGen.setSeed(randSeed)
  }
}
