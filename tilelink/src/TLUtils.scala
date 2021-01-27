package verif

import chisel3._
import chisel3.util.isPow2
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import verif.TLTransaction._

import scala.collection.immutable
import scala.collection.mutable
import scala.math.ceil

package object TLUtils {
  // Temporary location for parameters
  def defaultStandaloneSlaveParams: TLSlavePortParameters = TLSlavePortParameters.v1(Seq(TLSlaveParameters.v1(address = Seq(AddressSet(0x0, 0xfff)),
    supportsGet = TransferSizes(1, 32), supportsPutFull = TransferSizes(1, 32), supportsPutPartial = TransferSizes(1, 32),
    supportsLogical = TransferSizes(1, 32), supportsArithmetic = TransferSizes(1, 32), supportsHint = TransferSizes(1, 32),
    regionType = RegionType.UNCACHED)),
    beatBytes = 8)
  def defaultStandaloneMasterParams: TLMasterPortParameters = TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = "bundleBridgetoTL")))
  //    supportsProbe = TransferSizes(1, 32),
  //    supportsGet = TransferSizes(1, 32), supportsPutFull = TransferSizes(1, 32), supportsPutPartial = TransferSizes(1, 32),
  //    supportsLogical = TransferSizes(1, 32), supportsArithmetic = TransferSizes(1, 32), supportsHint = TransferSizes(1, 32))))
  def defaultVerifTLBundleParams: TLBundleParameters = TLBundleParameters(defaultStandaloneMasterParams, defaultStandaloneSlaveParams)
  // Temporary cache parameters
  def defaultStandaloneSlaveParamsCache: TLSlavePortParameters = TLSlavePortParameters.v1(Seq(TLSlaveParameters.v1(address = Seq(AddressSet(0x0, 0xfff)),
    supportsGet = TransferSizes(1, 32), supportsPutFull = TransferSizes(1, 32), supportsPutPartial = TransferSizes(1, 32),
    supportsLogical = TransferSizes(1, 32), supportsArithmetic = TransferSizes(1, 32), supportsHint = TransferSizes(1, 32),
    supportsAcquireB = TransferSizes(1, 32), supportsAcquireT = TransferSizes(1, 32),
    regionType = RegionType.UNCACHED)),
    endSinkId = 1, beatBytes = 8)
  def defaultStandaloneMasterParamsCache: TLMasterPortParameters = TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = "TestBundle",
    supportsProbe = TransferSizes(1, 32), supportsGet = TransferSizes(1, 32), supportsPutFull = TransferSizes(1, 32),
    supportsPutPartial = TransferSizes(1, 32), supportsLogical = TransferSizes(1, 32), supportsArithmetic = TransferSizes(1, 32),
    supportsHint = TransferSizes(1, 32))))
  def defaultVerifTLBundleParamsCache: TLBundleParameters = TLBundleParameters(defaultStandaloneMasterParamsCache, defaultStandaloneSlaveParamsCache)

  // Helper functions for message checking
  def aligned(data : UInt, base : UInt) : Boolean = {
    val dataI = data.litValue()
    val baseI = base.litValue() - 1
    ((dataI & baseI) == 0) && contiguous(baseI.U)
  }

  def alignedLg(data : UInt, base : UInt) : Boolean = {
    aligned(data, (1 << base.litValue().toInt).U)
  }

  def alignedMaskLg(mask : UInt, size : UInt) : Boolean = {
    (1 << (1 << size.litValue().toInt)) - 1 == mask.litValue().toInt
  }

  def contiguous(data : UInt) : Boolean = {
    val dataI = data.litValue()
    ((dataI + 1) & ~dataI) == (dataI + 1)
  }

  def contains(sizes: TransferSizes, x: UInt) : Boolean = {
    (x.litValue() >= sizes.min && x.litValue() <= sizes.max && isPow2(x.litValue()))
  }

  def containsLg(sizes: TransferSizes, lg: UInt) : Boolean = {
    contains(sizes, (1 << lg.litValue().toInt).U)
  }


  // Helper method to get size of TLChannel
  def getTLBundleDataSizeBytes (bnd : TLChannel): Int = {
    bnd match {
      case bndc: TLBundleA =>
        1 << bndc.size.litValue().toInt
      case bndc: TLBundleB =>
        1 << bndc.size.litValue().toInt
      case bndc: TLBundleC =>
        1 << bndc.size.litValue().toInt
      case bndc: TLBundleD =>
        1 << bndc.size.litValue().toInt
      case _: TLBundleE =>
        1
    }
  }

  // Helper method to see if transaction is non-Burst
  def isNonBurst (bnd : TLChannel): Boolean = {
    bnd match {
      case bndc: TLBundleA =>
        // Get, AcquireBlock, AcquirePerm
        if (bndc.opcode.litValue() == 4 || bndc.opcode.litValue() == 5 || bndc.opcode.litValue() == 6 || bndc.opcode.litValue() == 7) {
          return true
        }
        false
      case bndc: TLBundleB =>
        // Get, ProbeBlock, ProbePerm
        if (bndc.opcode.litValue() == 4 || bndc.opcode.litValue() == 5 || bndc.opcode.litValue() == 6 || bndc.opcode.litValue() == 7) {
          return true
        }
        false
      case bndc: TLBundleC =>
        // AccessAck, ProbeAck, Release
        if (bndc.opcode.litValue() == 0 || bndc.opcode.litValue() == 4 || bndc.opcode.litValue() == 6) {
          return true
        }
        false
      case bndc: TLBundleD =>
        // AccessAck, Grant, ReleaseAck
        if (bndc.opcode.litValue() == 0 || bndc.opcode.litValue() == 4 || bndc.opcode.litValue() == 6) {
          return true
        }
        false
      case _: TLBundleE =>
        // Always single message
        true
    }
  }

  // Helper function to map size -> # of beats
  def sizeToBeats (size : UInt, beatBytes : Int = 8): Int = {
    val sizeInt = 1 << size.litValue().toInt
    ceil(sizeInt / beatBytes.toDouble).toInt
  }

  // Helper method to test if given TLBundles (TLChannels) make up a complete TLTransaction
  def isCompleteTLTxn (txns: Seq[TLChannel]) : Boolean = {
    // Edge case
    if (txns.isEmpty) return false

    // TODO Hardcoded, update when configurability is added
    val beatBytes = 8
    val expectedTxnCount = if (isNonBurst(txns.head)) 1 else
      ceil(getTLBundleDataSizeBytes(txns.head) / beatBytes.toDouble).toInt

    // Currently only checks count
    // TODO Add checks for consistency (size, source, opcode, etc)
    txns.length == expectedTxnCount
  }

  // Helper method to get the next complete TLTransaction (list of TLBundles)
  // Returns (completeTxn, updatedSeq)
  def getNextCompleteTLTxn (txns: Seq[TLChannel]): Option[Seq[TLChannel]] = {
    // Edge case
    if (txns.isEmpty) return None

    // TODO Hardcoded, update when configurability is added
    val beatBytes = 8
    val expectedTxnCount = if (isNonBurst(txns.head)) 1 else
      ceil(getTLBundleDataSizeBytes(txns.head) / beatBytes.toDouble).toInt

    if (txns.length < expectedTxnCount) {
      None
    } else {
      Some(txns.dropRight(txns.size - expectedTxnCount))
    }
  }


  // Helper method to group together burst TLBundles
  def groupTLBundles (txns: List[TLChannel]) : List[List[TLChannel]] = {
    // TODO Hardcoded for now, update when configurability is added
    val beatBytes = 8
    val txnsLB = new mutable.ListBuffer[TLChannel]
    txnsLB ++= txns
    var result = new mutable.ListBuffer[List[TLChannel]]

    while (txnsLB.nonEmpty) {
      val txnCount = if (isNonBurst(txnsLB.head)) 1 else ceil(getTLBundleDataSizeBytes(txnsLB.head) / beatBytes.toDouble).toInt

      // Grouping txnCount TLTransactions (of the same type)
      val newList = new mutable.ListBuffer[TLChannel]
      var index = 0
      newList += txnsLB.remove(index)
      // Getting Class as "type"
      val classType = newList.head.getClass
      while (newList.length < txnCount) {
        if (txnsLB(index).getClass.equals(classType)) {
          newList += txnsLB.remove(index)
        } else {
          index += 1
        }
      }

      result += newList.toList
    }

    result.toList
  }

  // Arithmetic Helper Functions
  def max(a : UInt, b : UInt) : UInt = {
    if (a.litValue() < b.litValue()) b else a
  }

  def min(a : UInt, b : UInt) : UInt = {
    if (a.litValue() < b.litValue()) a else b
  }

  def maxu(a : UInt, b : UInt) : UInt = {
    // TODO Fix for unsigned
    if (a.litValue() < b.litValue()) b else a
  }

  def minu(a : UInt, b : UInt) : UInt = {
    // TODO Fix for unsigned
    if (a.litValue() < b.litValue()) a else b
  }

  def add(a : UInt, b : UInt) : UInt = {
    // Will overflow
    (a.litValue() + b.litValue()).U(64.W)
  }

  // Logistic Helper Functions
  def xor(a : UInt, b : UInt) : UInt = {
    (a.litValue() ^ b.litValue()).U(64.W)
  }

  def or(a : UInt, b : UInt) : UInt = {
    (a.litValue() | b.litValue()).U(64.W)
  }

  def and(a : UInt, b : UInt) : UInt = {
    (a.litValue() & b.litValue()).U(64.W)
  }

  // a must be the old value
  def swap(a : UInt, b : UInt) : UInt = {
    a
  }

  def toByteMask(mask : UInt) : BigInt = {
    var maskInt = mask.litValue()
    var result: BigInt = 0
    var i = 0
    do {
      if ((maskInt & 1) == 1) {
        result = result | 0xff << (i * 8)
      }
      maskInt = maskInt >> 1
      i = i + 1
    } while (maskInt > 0 )

    result
  }

  // Repeat Permissions for given size
  def permRepeater(size: UInt, perm: UInt, beatBytes : Int = 8) : List[UInt] = {
    val sizeInt = 1 << size.litValue().toInt
    var tempResult: BigInt = 0
    var resultList = mutable.ListBuffer[UInt]()
    var reset = true

    for (i <- 0 until sizeInt) {
      tempResult = (tempResult << 8) | perm.litValue()
      reset = false

      // Separating into separate beats
      if (i % beatBytes == (beatBytes - 1)) {
        resultList += tempResult.U((beatBytes * 8).W)
        tempResult = 0
        reset = true
      }
    }

    // Any dangling data
    if (!reset) {
      resultList += tempResult.U((beatBytes * 8).W)
    }

    resultList.toList
  }

  // State is a byte-addressed HashMap
  def readData(state: mutable.HashMap[Int,Int], size: UInt, address: UInt, mask: UInt, beatBytes: Int = 8): List[UInt] = {
    val sizeInt = 1 << size.litValue().toInt
    val addressInt = address.litValue().toInt
    val byteMask = toByteMask(mask)
    var tempResult: BigInt = 0
    var resultList = mutable.ListBuffer[UInt]()
    var reset = true

    for (i <- 0 until sizeInt) {
      tempResult = tempResult | (state.getOrElse(addressInt + i, 0) << (i * 8))
      reset = false

      // Separating into separate beats
      if (i % beatBytes == (beatBytes - 1)) {
        resultList += (tempResult & byteMask).U((beatBytes * 8).W)
        tempResult = 0
        reset = true
      }
    }

    // Any dangling data
    if (!reset) {
      resultList += (tempResult & byteMask).U((beatBytes * 8).W)
    }

    resultList.toList
  }

  // State is a byte-addressed HashMap
  def writeData(state: mutable.HashMap[Int,Int], size: UInt, address: UInt, datas: Seq[UInt], masks: Seq[UInt], beatBytes: Int = 8): Unit = {
    val sizeInt = 1 << size.litValue().toInt
    val addressInt = address.litValue().toInt
    var allData: BigInt = 0
    var allMask: BigInt = 0

    assert(datas.length == masks.length, "Data and Mask lists are not of equal lengths.")

    // Condensing all data and masks
    for (i <- 0 until datas.length) {
      allData = allData | (datas(i).litValue() << (beatBytes * 8 * i))
      allMask = allMask | (toByteMask(masks(i)) << (beatBytes * 8 * i))
    }

    // Writing all data with masks
    val byteMask = (1 << 8) - 1
    var preMaskData = 0
    var dataMask = 0
    for (i <- 0 until sizeInt) {
      // Getting a byte worth of data
      preMaskData = (allData & byteMask).toInt
      dataMask = (allMask & byteMask).toInt

      // Updating HashMap
      state(addressInt + i) = (state.getOrElse(addressInt + i, 0) & ~dataMask) | (preMaskData & dataMask)

      // Shifting to next byte
      allData  = allData >> 8
      allMask = allMask >> 8
    }
  }

  // TODO: refactor into one function
  case class SlaveMemoryState(txnBuffer: Seq[TLChannel], mem: immutable.Map[Int,Int])
  object SlaveMemoryState {
    def init(): SlaveMemoryState = { SlaveMemoryState(Seq(), immutable.HashMap[Int,Int]()) }
  }
  def testResponseWrapper(input: TLChannel, state: SlaveMemoryState, params: TLBundleParameters): (Seq[TLChannel], SlaveMemoryState) = {
    implicit val p = params

    // Collect transactions with the same opcode
    val newBuffer = state.txnBuffer :+ input
    if (isCompleteTLTxn(newBuffer)) {
      val (resp, newState) = testResponse(newBuffer, state.mem)
      (resp, SlaveMemoryState(Seq(), newState))
    } else {
      (Seq(), SlaveMemoryState(newBuffer, state.mem))
    }
  }

  def testResponse(input: Seq[TLChannel], state: immutable.Map[Int,Int])(implicit p: TLBundleParameters) : (Seq[TLChannel], immutable.Map[Int,Int]) = {
    val beatBytesSize = 3
    // Default response is corrupt transaction
    var responseTLTxn = Seq(AccessAck(denied=0, size=0, source=0))
    // Making internal copy of state (non-destructive)
    val state_int: mutable.HashMap[Int,Int] = mutable.HashMap[Int,Int]() ++ state

    // Assert
    assert(input.nonEmpty, "ERROR: List of TLBundles is EMPTY for testResponse slaving function.")


    val inputhead = input.head
    // Transaction response
    inputhead match {
      case txnc: TLBundleA =>
        txnc.opcode.litValue().toInt match {
          case TLOpcodes.Get =>
            // Assert
            assert(input.size == 1, "ERROR: Get request has too many beats.")

            // Read Data
            val readOut = readData(state = state_int, size = txnc.size, address  = txnc.address, mask = txnc.mask)

            if (txnc.size.litValue() > beatBytesSize)
              responseTLTxn = AccessAckDataBurst(source = txnc.source.litValue().toInt, denied = 0, data = readOut.map(_.litValue()))
            else
              responseTLTxn = Seq(AccessAckData(readOut.head.litValue(), 0, txnc.source.litValue().toInt))

          case TLOpcodes.PutFullData | TLOpcodes.PutPartialData =>
            val size = if (txnc.size.litValue().toInt > beatBytesSize) beatBytesSize.U else txnc.size
            val source = txnc.source
            val address = txnc.address
            val datas = input.map(_.asInstanceOf[TLBundleA].data)
            val masks = input.map(_.asInstanceOf[TLBundleA].mask)


            // Write Data
            writeData(state = state_int, size = size, address = address, datas = datas, masks = masks)

            // Response
            responseTLTxn = Seq(AccessAck(size = size.litValue().toInt, source = source.litValue().toInt, denied = 0))

          case TLOpcodes.ArithmeticData =>
            val size = if (txnc.size.litValue().toInt > beatBytesSize) beatBytesSize.U else txnc.size
            val param = txnc.param
            val source = txnc.source
            val address = txnc.address
            val datas = input.map(_.asInstanceOf[TLBundleA].data)
            val masks = input.map(_.asInstanceOf[TLBundleA].mask)

            var function : (UInt, UInt) => UInt = min
            param.litValue().toInt match {
              case 0 => function = min
              case 1 => function = max
              case 2 => function = minu
              case 3 => function = maxu
              case 4 => function = add
            }

            // Reading Old Data (to return)
            val oldData = readData(state = state_int, size = size, address = address, mask = 0xff.U)

            // Creating newData (to write)
            var newData = mutable.ListBuffer[UInt]()
            for (((n, m), o) <- (datas zip masks) zip oldData) {
              newData += function(o, (n.litValue() & toByteMask(m)).U)
            }

            writeData(state = state_int, size = txnc.size, address = txnc.address, datas = newData.toList, masks = List.fill(newData.length)(0xff.U))
            responseTLTxn = AccessAckDataBurst(source = source.litValue().toInt, denied = 0, data = oldData.map(_.litValue()))

          case TLOpcodes.LogicalData =>
            val size = if (txnc.size.litValue().toInt > beatBytesSize) beatBytesSize.U else txnc.size
            val param = txnc.param
            val source = txnc.source
            val address = txnc.address
            val datas = input.map(_.asInstanceOf[TLBundleA].data)
            val masks = input.map(_.asInstanceOf[TLBundleA].mask)

            var function : (UInt, UInt) => UInt = min
            param.litValue().toInt match {
              case 0 => function = xor
              case 1 => function = or
              case 2 => function = and
              case 3 => function = swap
            }

            // Reading Old Data (to return)
            val oldData = readData(state = state_int, size = txnc.size, address = txnc.address, mask = 0xff.U)

            // Creating newData (to write)
            var newData = mutable.ListBuffer[UInt]()
            for (((n, m), o) <- (datas zip masks) zip oldData) {
              newData += function(o, (n.litValue() & toByteMask(m)).U)
            }

            writeData(state = state_int, size = size, address = address, datas = newData.toList, masks = List.fill(newData.length)(0xff.U))
            responseTLTxn = AccessAckDataBurst(source = source.litValue().toInt, denied = 0, data = oldData.map(_.litValue()))

          case TLOpcodes.Hint =>
            // Currently don't accept hints
            responseTLTxn = Seq(HintAck(size = txnc.size.litValue().toInt, source = txnc.source.litValue().toInt, denied = 1))
        }
      case _ => ???
    }
    (responseTLTxn, state_int.toMap)
  }

  // Wrapper class that stores mapping of Address --> Permissions (Note: is block aligned)
  // Permissions: 0 - None, 1 - Read (Branch), 2 - Read/Write (Tip), -1 - Waiting for Grant/Ack
  class RWPermState(blockSize: Int = 3) {
    // HashMap implementation
    private val intState = mutable.HashMap[Int,Int]()
    private val mask = ~((1 << blockSize) - 1)

    def getPerm(address: Int): Int = { intState.getOrElse(address & mask, 0) }

    def setPerm(address: Int, permission: Int): Unit = {
      intState(address & mask) = permission
    }

    def getAllAddr: List[Int] = {
      intState.keys.toList
    }

    // For debugging
    def getState: mutable.HashMap[Int,Int] = {
      intState
    }
  }
}
