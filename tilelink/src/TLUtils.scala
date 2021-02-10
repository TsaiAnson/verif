package verif

import chisel3._
import chisel3.util.isPow2
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import scala.collection.mutable
import scala.math.ceil

package object TLUtils {
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
        (bndc.opcode.litValue() == 4 || bndc.opcode.litValue() == 5 || bndc.opcode.litValue() == 6 || bndc.opcode.litValue() == 7)
      case bndc: TLBundleB =>
        // Get, ProbeBlock, ProbePerm
        (bndc.opcode.litValue() == 4 || bndc.opcode.litValue() == 5 || bndc.opcode.litValue() == 6 || bndc.opcode.litValue() == 7)
      case bndc: TLBundleC =>
        // AccessAck, ProbeAck, Release
        (bndc.opcode.litValue() == 0 || bndc.opcode.litValue() == 4 || bndc.opcode.litValue() == 6)
      case bndc: TLBundleD =>
        // AccessAck, Grant, ReleaseAck
        (bndc.opcode.litValue() == 0 || bndc.opcode.litValue() == 4 || bndc.opcode.litValue() == 6)
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
  def isCompleteTLTxn (txns: Seq[TLChannel], beatBytes: Int = 8) : Boolean = {
    // Edge case
    if (txns.isEmpty) return false

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
