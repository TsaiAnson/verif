package verif

import chisel3._
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tilelink.TLSlaveParameters

import scala.math.pow
import scala.util.Random
import scala.collection.mutable.{ListBuffer, HashMap}

// Currently supports TL-UL, TL-UH, TL-C (some restrictions in randomization)
class SWTLFuzzer (params : TLSlaveParameters, overrideAddr: Option[AddressSet] = None, beatSize : Int = 3,
                 // TL-UH
                  burst : Boolean = false, arith : Boolean = false, logic : Boolean = false, hints : Boolean = false,
                 // TL-C
                  tlc : Boolean = false, cacheBlockSize : Int = 3, acquire : Boolean = false,
                 // Randomization
                  randSeed : Int = 1234567890) {

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
    val intState = HashMap[Int,Int]()

    // Generating Transactions
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

        if (tlc) {
          typeTxn = randGen.nextInt.abs % 8
        } else {
          typeTxn = randGen.nextInt.abs % 6
        }

        println(typeTxn)

        // Need to "re-roll" for unwanted txn types
        redo = false
        if (!arith && typeTxn == 3) {
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
        address = getRandomLegalAddress(params.address)
        mask = (1 << (1 << beatSize)) - 1

        genTxns += Get(size = size.U, source = source.U, address.U(64.W), mask = mask.U)

      } else if (typeTxn == 1) {
        // PutFull
        size = randGen.nextInt(5) + 1
        address = getRandomLegalAddress(params.address)
        if (size > beatSize) {
          mask = (1 << (1 << beatSize)) - 1
        } else {
          mask = (1 << (1 << size)) - 1
        }

        if (size > beatSize && burst) {
          val beatCount = 1 << (size - beatSize)
          genTxns += PutFullBurst(size = size.U, source = source.U, address.U(64.W), masks = List.fill(beatCount)(mask.U),
            datas = List.fill(beatCount)(randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt).U(64.W)))
        } else {
          data = randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt) // Will account for transfer sizes
          genTxns += PutFull(source = source.U, addr = address.U(64.W), mask = mask.U, data = data.U(64.W)) // Hardcoded mask for now
        }

      } else if (typeTxn == 2) {
        // PutPartial
        size = randGen.nextInt(5) + 1
        address = getRandomLegalAddress(params.address)
        mask = randGen.nextInt((1 << (1 << beatSize)))

        if (size > beatSize && burst) {
          val beatCount = 1 << (size - beatSize)
          genTxns += PutFullBurst(size = size.U, source = source.U, address.U(64.W), masks = List.fill(beatCount)((randGen.nextInt(mask) + 1).U),
            datas = List.fill(beatCount)(randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt).U(64.W)))
        } else {
          data = randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt) // Will account for transfer sizes
          genTxns += PutFull(source = source.U, addr = address.U(64.W), mask = mask.U, data = data.U(64.W))
        }

      } else if (typeTxn == 3) {
        // ArithData
        param = randGen.nextInt(5)
        size = randGen.nextInt(5) + 1
        address = getRandomLegalAddress(params.address)
        if (size > beatSize) {
          mask = (1 << (1 << beatSize)) - 1
        } else {
          mask = (1 << (1 << size)) - 1
        }

        if (size > beatSize && burst) {
          val beatCount = 1 << (size - beatSize)
          genTxns += ArithDataBurst(param = param.U, size = size.U, source = source.U, address.U(64.W), masks = List.fill(beatCount)(mask.U),
            datas = List.fill(beatCount)(randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt).U(64.W)))
        } else {
          data = randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt) // Will account for transfer sizes
          genTxns += ArithData(param = param.U, source = source.U, addr = address.U(64.W), mask = mask.U, data = data.U(64.W)) // Hardcoded mask for now
        }

      } else if (typeTxn == 4) {
        // LogicData
        param = randGen.nextInt(4)
        size = randGen.nextInt(5) + 1
        address = getRandomLegalAddress(params.address)
        if (size > beatSize) {
          mask = (1 << (1 << beatSize)) - 1
        } else {
          mask = (1 << (1 << size)) - 1
        }

        if (size > beatSize) {
          val beatCount = 1 << (size - beatSize)
          genTxns += LogicDataBurst(param = param.U,size = size.U, source = source.U, address.U(64.W), masks = List.fill(beatCount)(mask.U),
            datas = List.fill(beatCount)(randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt).U(64.W)))
        } else {
          data = randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt) // Will account for transfer sizes
          genTxns += LogicData(param = param.U, source = source.U, addr = address.U(64.W), mask = mask.U, data = data.U(64.W)) // Hardcoded mask for now
        }

      } else if (typeTxn == 5) {
        // Intent
        param = randGen.nextInt(2)
        size = randGen.nextInt(5) + 1
        address = getRandomLegalAddress(params.address)
        if (size > beatSize) {
          mask = (1 << (1 << beatSize)) - 1
        } else {
          mask = (1 << (1 << size)) - 1
        }

        // Unsure how to indicate burst mask...
        genTxns += Intent(param = param.U, size = size.U, source = source.U, addr = address.U, mask = mask.U)

        if (param == 0) {
          genTxns += Get(size = size.U, source = source.U, address.U(64.W), mask = mask.U)
        } else {
          val fullPartial = randGen.nextInt(2)
          if (size > beatSize && burst) {
            val beatCount = 1 << (size - beatSize)
            if (fullPartial == 0) {
              genTxns += PutFullBurst(size = size.U, source = source.U, address.U(64.W), masks = List.fill(beatCount)(mask.U),
                datas = List.fill(beatCount)(randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt).U(64.W)))
            } else {
              genTxns += PutPartialBurst(size = size.U, source = source.U, address.U(64.W), masks = List.fill(beatCount)(mask.U),
                datas = List.fill(beatCount)(randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt).U(64.W)))
            }
          } else {
            data = randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt) // Will account for transfer sizes
            if (fullPartial == 0) {
              genTxns += PutFull(source = source.U, addr = address.U(64.W), mask = mask.U, data = data.U(64.W)) // Hardcoded mask for now
            } else {
              genTxns += PutPartial(source = source.U, addr = address.U(64.W), mask = mask.U, data = data.U(64.W))
            }
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

          address = getRandomLegalAddress(params.address)
          // Getting block-aligned address
          address = address & ~((1 << cacheBlockSize) - 1)

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
            genTxns += AcquireBlock(param = param.U, size = size.U, source = source.U, addr = address.U, mask = mask.U)
          } else {
            genTxns += AcquirePerm(param = param.U, size = size.U, source = source.U, addr = address.U, mask = mask.U)
          }
        } else {
          // Converting to releaseData (since old permissions must be 2 for redo)
          param = randGen.nextInt(2) // TtoB or TtoN
          genTxns += ReleaseDataBurst(param = param.U, size = size.U, source = source.U, addr = address.U,
            datas = List.fill(1 << (cacheBlockSize - beatSize))(randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt).U(64.W)))
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
            address = getRandomLegalAddress(params.address)
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
              param = 3
            }
          }
        }

        // If no valid address to release, do acquire instead so that next time there will be a release
        if (repeats != 10) {
          if (param < 3) {
            // Dirty data
            genTxns += ReleaseDataBurst(param = param.U, size = size.U, source = source.U, addr = address.U,
              datas = List.fill(1 << (cacheBlockSize - beatSize))(randGen.nextInt(pow(2, (1 << beatSize) * 8).toInt).U(64.W)))
          } else {
            genTxns += Release(param = param.U, size = size.U, source = source.U, addr = address.U)
          }
        } else {
          // Converting to Acquire
          param = randGen.nextInt(2) // NtoB or NtoT
          if (randGen.nextInt() % 2 == 1) {
            genTxns += AcquireBlock(param = param.U, size = size.U, source = source.U, addr = address.U, mask = mask.U)
          } else {
            genTxns += AcquirePerm(param = param.U, size = size.U, source = source.U, addr = address.U, mask = mask.U)
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
