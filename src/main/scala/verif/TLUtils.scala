package verif

import chisel3._
import chisel3.util.isPow2
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import chisel3.experimental.BundleLiterals._

import scala.collection.mutable.{ListBuffer, Queue, HashMap}
import scala.math.ceil

trait Transaction extends IgnoreSeqInBundle { this: Bundle =>
  override def equals(that: Any): Boolean = {
    var result = this.getClass() == that.getClass()
    if (result) {
      val fields = this.getClass.getDeclaredFields
      for (field <- fields) {
        field.setAccessible(true)
        if (field.get(this).isInstanceOf[List[UInt]]) {
          result &= field.get(this).asInstanceOf[List[UInt]].map((x: UInt) => x.litValue()).sameElements(
            field.get(that).asInstanceOf[List[UInt]].map((x: UInt) => x.litValue()))
        } else {
          result &= field.get(this).asInstanceOf[Data].litValue() == field.get(that).asInstanceOf[Data].litValue()
        }
      }
    }
    result
  }

  override def toString(): String = {
    var result = this.className + "("

    val fields = this.getClass.getDeclaredFields
    for (field <- fields) {
      field.setAccessible(true)
      if (field.get(this).isInstanceOf[List[UInt]]) {
        result += field.getName + ": ("
        for (u <- field.get(this).asInstanceOf[List[UInt]]) {
          result += u.litValue().toString() + ", "
        }
        result = result.slice(0, result.length - 2) + "), "
      } else {
        result += field.getName + ": "+ field.get(this).asInstanceOf[Data].litValue().toString() + ", "
      }
    }
    result = result.slice(0, result.length - 2) + ")"

    result
  }
}

// TODO Add source/sink fields when working with buses (refactor source fields)
// *Burst for burst (multi-beat) operations
sealed trait TLTransaction extends Bundle with Transaction
case class Get(size: UInt, addr: UInt, mask: UInt, source: UInt = 0.U) extends TLTransaction
case class PutFull(addr: UInt, mask: UInt, data: UInt, source: UInt = 0.U) extends TLTransaction
case class PutFullBurst(size: UInt, addr: UInt, masks: List[UInt], datas: List[UInt], source: UInt = 0.U) extends TLTransaction
case class PutPartial(addr: UInt, mask: UInt, data: UInt, source: UInt = 0.U) extends TLTransaction
case class PutPartialBurst(size: UInt, addr: UInt, masks: List[UInt], datas: List[UInt], source: UInt = 0.U) extends TLTransaction
case class ArithData(param: UInt, addr: UInt, mask: UInt, data: UInt, source: UInt = 0.U) extends TLTransaction
case class ArithDataBurst(param: UInt, size: UInt, addr: UInt, masks: List[UInt], datas: List[UInt], source: UInt = 0.U) extends TLTransaction
case class LogicData(param: UInt, addr: UInt, mask: UInt, data: UInt, source: UInt = 0.U) extends TLTransaction
case class LogicDataBurst(param: UInt, size: UInt, addr: UInt, masks: List[UInt], datas: List[UInt], source: UInt = 0.U) extends TLTransaction
case class Intent(param: UInt, size: UInt, addr: UInt, mask: UInt, source: UInt = 0.U) extends TLTransaction
case class AccessAck(size: UInt, denied: Bool, source: UInt = 0.U) extends TLTransaction
case class AccessAckData(size: UInt, denied: Bool, data: UInt, source: UInt = 0.U) extends TLTransaction
case class AccessAckDataBurst(size: UInt, denied: Bool, datas: List[UInt], source: UInt = 0.U) extends TLTransaction
case class HintAck(size: UInt, denied: Bool, source: UInt = 0.U) extends TLTransaction

package object verifTLUtils {
  // Temporary location for parameters
  def standaloneSlaveParams: TLSlavePortParameters = TLSlavePortParameters.v1(Seq(TLSlaveParameters.v1(address = Seq(AddressSet(0x0, 0xfff)),
    supportsGet = TransferSizes(1, 32), supportsPutFull = TransferSizes(1, 32), supportsPutPartial = TransferSizes(1, 32),
    supportsLogical = TransferSizes(1, 32), supportsArithmetic = TransferSizes(1, 32), supportsHint = TransferSizes(1, 32))), beatBytes = 8)
  def standaloneMasterParams: TLMasterPortParameters = TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = "bundleBridgetoTL")))
  //    supportsProbe = TransferSizes(1, 32),
  //    supportsGet = TransferSizes(1, 32), supportsPutFull = TransferSizes(1, 32), supportsPutPartial = TransferSizes(1, 32),
  //    supportsLogical = TransferSizes(1, 32), supportsArithmetic = TransferSizes(1, 32), supportsHint = TransferSizes(1, 32))))
  def verifTLBundleParams: TLBundleParameters = TLBundleParameters(standaloneMasterParams, standaloneSlaveParams)

  //  def verifTLUBundleParams: TLBundleParameters = TLBundleParameters(addressBits = 64, dataBits = 64, sourceBits = 1,
  //    sinkBits = 1, sizeBits = 6,
  //    echoFields = Seq(), requestFields = Seq(), responseFields = Seq(),
  //    hasBCE = false)
  //  def verifTLCBundleParams: TLBundleParameters = TLBundleParameters(addressBits = 64, dataBits = 64, sourceBits = 1,
  //    sinkBits = 1, sizeBits = 6,
  //    echoFields = Seq(), requestFields = Seq(), responseFields = Seq(),
  //    hasBCE = true)

  def TLUBundleAHelper (opcode: UInt = 0.U, param: UInt = 0.U, size: UInt = 3.U(3.W), source: UInt = 1.U, address: UInt = 0.U,
                        mask: UInt = 0xff.U, data: UInt = 0.U, corrupt: Bool = false.B) : TLBundleA = {
    //    assert(verifTLBundleParams.sizeBits >= size.getWidth)
    //    println(s"${verifTLBundleParams.sizeBits}, ${size.getWidth}")
    new TLBundleA(verifTLBundleParams).Lit(_.opcode -> opcode, _.param -> param, _.size -> size, _.source -> source,
      _.address -> address, _.mask -> mask, _.data -> data, _.corrupt -> corrupt)
  }

  def TLUBundleBHelper (opcode: UInt = 0.U, param: UInt = 0.U, size: UInt = 2.U, source: UInt = 1.U, address: UInt = 0.U,
                        mask: UInt = 0xff.U, data: UInt = 0.U, corrupt: Bool = false.B) : TLBundleB = {
    new TLBundleB(verifTLBundleParams).Lit(_.opcode -> opcode, _.param -> param, _.size -> size, _.source -> source,
      _.address -> address, _.mask -> mask, _.data -> data, _.corrupt -> corrupt)
  }

  def TLUBundleCHelper (opcode: UInt = 0.U, param: UInt = 0.U, size: UInt = 2.U, source: UInt = 1.U, address: UInt = 0.U,
                        data: UInt = 0.U, corrupt: Bool = false.B) : TLBundleC = {
    new TLBundleC(verifTLBundleParams).Lit(_.opcode -> opcode, _.param -> param, _.size -> size, _.source -> source,
      _.address -> address, _.data -> data, _.corrupt -> corrupt)
  }

  def TLUBundleDHelper (opcode: UInt = 0.U, param: UInt = 0.U, size: UInt = 3.U, source: UInt = 1.U, sink: UInt = 0.U,
                        data: UInt = 0.U, denied: Bool = false.B, corrupt: Bool = false.B) : TLBundleD = {
    new TLBundleD(verifTLBundleParams).Lit(_.opcode -> opcode, _.param -> param, _.size -> size, _.source -> source,
      _.sink -> sink, _.data -> data, _.denied -> denied, _.corrupt -> corrupt)
  }

  def TLUBundleEHelper (sink: UInt = 0.U) : TLBundleE = {
    new TLBundleE(verifTLBundleParams).Lit(_.sink -> sink)
  }

  // Helper functions for message checking
  def aligned(data : UInt, base : UInt) : Boolean = {
    val dataI = data.litValue()
    val baseI = base.litValue() - 1
    ((dataI & baseI) == 0) && contiguous(baseI.U)
  }

  def alignedLg(data : UInt, base : UInt) : Boolean = {
    aligned(data, (1 << base.litValue().toInt).U)
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

  // UGRADED FROM TLTransactiontoTLBundle
  def TLTransactiontoTLBundles(txn: TLTransaction): List[TLChannel] = {
    var result = new ListBuffer[TLChannel]()
    txn match {
      case _: PutFull =>
        val txnc = txn.asInstanceOf[PutFull]
        result += TLUBundleAHelper(opcode = 0.U, source = txnc.source, address = txnc.addr, data = txnc.data)
      case _: PutFullBurst =>
        val txnc = txn.asInstanceOf[PutFullBurst]
        for ((m, d) <- (txnc.masks zip txnc.datas)) {
          result += TLUBundleAHelper(opcode = 0.U, size = txnc.size, source = txnc.source, address = txnc.addr, mask = m, data = d)
        }
      case _: PutPartial =>
        val txnc = txn.asInstanceOf[PutPartial]
        result += TLUBundleAHelper(opcode = 0.U, source = txnc.source, address = txnc.addr, mask = txnc.mask, data = txnc.data)
      case _: PutPartialBurst =>
        val txnc = txn.asInstanceOf[PutPartialBurst]
        for ((m, d) <- (txnc.masks zip txnc.datas)) {
          result += TLUBundleAHelper(opcode = 0.U, size = txnc.size, source = txnc.source, address = txnc.addr, mask = m, data = d)
        }
      case _: ArithData =>
        val txnc = txn.asInstanceOf[ArithData]
        result += TLUBundleAHelper(opcode = 2.U, param = txnc.param, source = txnc.source, address = txnc.addr, mask = txnc.mask, data = txnc.data)
      case _: ArithDataBurst =>
        val txnc = txn.asInstanceOf[ArithDataBurst]
        for ((m, d) <- (txnc.masks zip txnc.datas)) {
          result += TLUBundleAHelper(opcode = 2.U, param = txnc.param, size = txnc.size, source = txnc.source, address = txnc.addr, mask = m, data = d)
        }
      case _: LogicData =>
        val txnc = txn.asInstanceOf[LogicData]
        result += TLUBundleAHelper(opcode = 3.U, param = txnc.param, source = txnc.source, address = txnc.addr, mask = txnc.mask, data = txnc.data)
      case _: LogicDataBurst =>
        val txnc = txn.asInstanceOf[LogicDataBurst]
        for ((m, d) <- (txnc.masks zip txnc.datas)) {
          result += TLUBundleAHelper(opcode = 3.U, param = txnc.param, size = txnc.size, source = txnc.source, address = txnc.addr, mask = m, data = d)
        }
      case _: Get =>
        val txnc = txn.asInstanceOf[Get]
        result += TLUBundleAHelper(opcode = 4.U, size = txnc.size, source = txnc.source, address = txnc.addr)
      case _: Intent =>
        val txnc = txn.asInstanceOf[Intent]
        result += TLUBundleAHelper(opcode = 5.U, param = txnc.param, size = txnc.size, source = txnc.source, address = txnc.addr, mask = txnc.mask)
      case _: AccessAck =>
        val txnc = txn.asInstanceOf[AccessAck]
        result += TLUBundleDHelper(source = txnc.source)
      case _: AccessAckData =>
        val txnc = txn.asInstanceOf[AccessAckData]
        result += TLUBundleDHelper(opcode = 1.U, source = txnc.source, data = txnc.data)
      case _: AccessAckDataBurst =>
        val txnc = txn.asInstanceOf[AccessAckDataBurst]
        for (d <- txnc.datas) {
          result += TLUBundleDHelper(opcode = 1.U, size = txnc.size, source = txnc.source, data = d)
        }
      case _: HintAck =>
        val txnc = txn.asInstanceOf[HintAck]
        result += TLUBundleDHelper(opcode = 2.U, size = txnc.size, source = txnc.source)
    }
    result.toList
  }

  // Helper method to get size of TLChannel
  def getTLBundleDataSizeBytes (bnd : TLChannel): Int = {
    bnd match {
      case _: TLBundleA =>
        val bndc = bnd.asInstanceOf[TLBundleA]
        1 << bndc.size.litValue().toInt
      case _: TLBundleD =>
        val bndc = bnd.asInstanceOf[TLBundleD]
        1 << bndc.size.litValue().toInt
    }
  }

  // Helper method to see if transaction is non-Burst
  // Only checking A and D bundles
  def isNonBurst (bnd : TLChannel): Boolean = {
    bnd match {
      case _: TLBundleA =>
        val bndc = bnd.asInstanceOf[TLBundleA]
        if (bndc.opcode.litValue() == 4) {
          return true
        }
        false
      case _: TLBundleD =>
        val bndc = bnd.asInstanceOf[TLBundleD]
        if (bndc.opcode.litValue() == 0) {
          return true
        }
        false
    }
  }

  // Helper method to test if given TLBundles (TLChannels) make up a complete TLTransaction
  def isCompleteTLTxn (txns: List[TLChannel]) : Boolean = {
    // TODO Hardcoded, update when configurability is added
    val beatBytes = 8
    val expectedTxnCount = if (isNonBurst(txns.head)) 1 else
      ceil(getTLBundleDataSizeBytes(txns.head) / beatBytes.toDouble).toInt

    // Currently only checks count
    // TODO Add checks for consistency (size, source, opcode, etc)
    txns.length == expectedTxnCount
  }

  // Helper method to group together burst TLBundles
  def groupTLBundles (txns: List[TLChannel]) : List[List[TLChannel]] = {
    // TODO Hardcoded for now, update when configurability is added
    val beatBytes = 8
    val txnsLB = new ListBuffer[TLChannel]
    txnsLB ++= txns
    var result = new ListBuffer[List[TLChannel]]

    while (txnsLB.nonEmpty) {
      val txnCount = if (isNonBurst(txnsLB.head)) 1 else ceil(getTLBundleDataSizeBytes(txnsLB.head) / beatBytes.toDouble).toInt

      // Grouping txnCount TLTransactions (of the same type)
      val newList = new ListBuffer[TLChannel]
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

  // UPDGRADED FROM TLBundletoTLTransaction
  def TLBundlestoTLTransaction(bnds : List[TLChannel], TLSParam : TLSlaveParameters = standaloneSlaveParams.managers(0) ) : TLTransaction = {
    val bndsq = new Queue[TLChannel]()
    bndsq ++= bnds
    val bnd = bndsq.dequeue()

    bnd match {
      case _: TLBundleA =>
        val bndc = bnd.asInstanceOf[TLBundleA]
        if (bndc.opcode.litValue() == 0) {

          // Assertions checking on first TLBundle
          assert(TLSParam.supportsPutFull != TransferSizes.none, "Channel does not support PUTFULL requests.")
          assert(bndc.param.litValue() == 0, "Non-zero param field for PUTFULL TLBundle")
          // Only for TL-UL
          //          assert(containsLg(TLSParam.supportsPutFull, bndc.size), "Size is outside of valid transfer sizes")
          assert(alignedLg(bndc.address, bndc.size), s"PUTFULL Address (${bndc.address}) is NOT aligned with size (${bndc.size})")
          // TODO FIX, MASK IS BYTE BASED
          //          assert(alignedLg(bndc.mask, bndc.size), s"PUTFULL MASK (${bndc.mask}) is not aligned with size (${bndc.size})")
          assert(contiguous(bndc.mask), "PUTFULL MASK is not contiguous")
          assert(!bndc.corrupt.litToBoolean, "Corrupt PUTFULL TLBundle")

          // If bundles are in a burst
          if (bndsq.size > 0) {
            var masks = new ListBuffer[UInt]
            var datas = new ListBuffer[UInt]
            masks += bndc.mask
            datas += bndc.data

            while (bndsq.nonEmpty) {
              // Checking if other bundles in burst are valid
              val other_bndc = bndsq.dequeue().asInstanceOf[TLBundleA]

              assert(bndc.address.litValue() == other_bndc.address.litValue())
              assert(bndc.param.litValue() == other_bndc.param.litValue())
              assert(bndc.size.litValue() == other_bndc.size.litValue())

              masks += other_bndc.mask
              datas += other_bndc.data
            }
            PutFullBurst(size = bndc.size, source = bndc.source, addr = bndc.address, masks = masks.toList, datas = datas.toList)
          } else {
            PutFull(source = bndc.source, addr = bndc.address, mask = bndc.mask, data = bndc.data)
          }
        } else if (bndc.opcode.litValue() == 1) {

          // Assertions checking on first TLBundle
          assert(TLSParam.supportsPutPartial != TransferSizes.none, "Channel does not support PUTPARTIAL requests.")
          assert(bndc.param.litValue() == 0, "Non-zero param field for PUTPARTIAL TLBundle")
          // Only for TL-UL
          //          assert(containsLg(TLSParam.supportsPutPartial, bndc.size), "Size is outside of valid transfer sizes")
          assert(alignedLg(bndc.address, bndc.size), s"PUTPARTIAL Address (${bndc.address}) is NOT aligned with size (${bndc.size})")
          // TODO Check that high bits are aligned
          assert(!bndc.corrupt.litToBoolean, "Corrupt PUTPARTIAL TLBundle")

          // If bundles are in a burst
          if (bndsq.size > 0) {
            var masks = new ListBuffer[UInt]
            var datas = new ListBuffer[UInt]
            masks += bndc.mask
            datas += bndc.data

            while (bndsq.nonEmpty) {
              // Checking if other bundles in burst are valid
              val other_bndc = bndsq.dequeue().asInstanceOf[TLBundleA]

              assert(bndc.address.litValue() == other_bndc.address.litValue())
              assert(bndc.param.litValue() == other_bndc.param.litValue())
              assert(bndc.size.litValue() == other_bndc.size.litValue())

              masks += other_bndc.mask
              datas += other_bndc.data
            }
            PutPartialBurst(size = bndc.size, source = bndc.source, addr = bndc.address, masks = masks.toList, datas = datas.toList)
          } else {
            PutPartial(source = bndc.source, addr = bndc.address, mask = bndc.mask, data = bndc.data)
          }

        } else if (bndc.opcode.litValue() == 2) {

          // Assertions checking on first TLBundle
          assert(TLSParam.supportsArithmetic != TransferSizes.none, "Channel does not support ARITHMETIC requests.")
          assert(bndc.param.litValue() > 0 && bndc.param.litValue() <= 4, s"Non-valid PARAM (${bndc.param}) for ARITHMETIC Data Bundle")
          assert(alignedLg(bndc.address, bndc.size), s"ARITHMETIC Address (${bndc.address}) is NOT aligned with size (${bndc.size})")
          // TODO Add checks for mask
          assert(!bndc.corrupt.litToBoolean, "Corrupt ARITHMETIC TLBundle")

          // If bundles are in a burst
          if (bndsq.size > 0) {
            var masks = new ListBuffer[UInt]
            var datas = new ListBuffer[UInt]
            masks += bndc.mask
            datas += bndc.data

            while (bndsq.nonEmpty) {
              // Checking if other bundles in burst are valid
              val other_bndc = bndsq.dequeue().asInstanceOf[TLBundleA]

              assert(bndc.address.litValue() == other_bndc.address.litValue())
              assert(bndc.param.litValue() == other_bndc.param.litValue())
              assert(bndc.size.litValue() == other_bndc.size.litValue())

              masks += other_bndc.mask
              datas += other_bndc.data
            }
            ArithDataBurst(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address, masks = masks.toList,
              datas = datas.toList)
          } else {
            ArithData(param = bndc.param, source = bndc.source, addr = bndc.address, mask = bndc.mask, data = bndc.data)
          }

        } else if (bndc.opcode.litValue() == 3) {

          // Assertions checking on first TLBundle
          assert(TLSParam.supportsLogical != TransferSizes.none, "Channel does not support LOGIC requests.")
          assert(bndc.param.litValue() > 0 && bndc.param.litValue() <= 3, s"Non-valid PARAM (${bndc.param}) for LOGIC Data Bundle")
          assert(alignedLg(bndc.address, bndc.size), s"LOGIC Address (${bndc.address}) is NOT aligned with size (${bndc.size})")
          // TODO Add checks for mask
          assert(!bndc.corrupt.litToBoolean, "Corrupt LOGICAL TLBundle")

          // If bundles are in a burst
          if (bndsq.size > 0) {
            var masks = new ListBuffer[UInt]
            var datas = new ListBuffer[UInt]
            masks += bndc.mask
            datas += bndc.data

            while (bndsq.nonEmpty) {
              // Checking if other bundles in burst are valid
              val other_bndc = bndsq.dequeue().asInstanceOf[TLBundleA]

              assert(bndc.address.litValue() == other_bndc.address.litValue())
              assert(bndc.param.litValue() == other_bndc.param.litValue())
              assert(bndc.size.litValue() == other_bndc.size.litValue())

              masks += other_bndc.mask
              datas += other_bndc.data
            }
            LogicDataBurst(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address, masks = masks.toList,
              datas = datas.toList)
          } else {
            LogicData(param = bndc.param, source = bndc.source, addr = bndc.address, mask = bndc.mask, data = bndc.data)
          }

        } else if (bndc.opcode.litValue() == 4) {

          // Assertions checking on first TLBundle
          assert(TLSParam.supportsGet != TransferSizes.none, "Channel does not support GET requests.")
          assert(bndc.param.litValue() == 0, "Non-zero param field for GET TLBundle")
          // Only for TL-UL
          //          assert(containsLg(TLSParam.supportsGet, bndc.size), "Size is outside of valid transfer sizes")
          // Need to check
          //          assert(alignedLg(bndc.mask, bndc.size), "GET MASK is not aligned")
          assert(contiguous(bndc.mask), "GET MASK is not contiguous")
          assert(!bndc.corrupt.litToBoolean, "Corrupt GET TLBundle")
          Get(size = bndc.size, source = bndc.source, mask = bndc.mask, addr = bndc.address)

        } else if (bndc.opcode.litValue() == 5) {

          // Assertions checking on first TLBundle
          assert(TLSParam.supportsHint != TransferSizes.none, "Channel does not support INTENT requests.")
          assert(bndc.param.litValue() > 1, s"Non-valid PARAM (${bndc.param}) for INTENT Data Bundle")
          // Need to check
          //          assert(alignedLg(bndc.mask, bndc.size), "GET MASK is not aligned")
          assert(!bndc.corrupt.litToBoolean, "Corrupt INTENT TLBundle")

          Intent(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address, mask = bndc.mask)

        } else {

          assert(false, "Invalid OPCODE on A Channel")
          Get(size = 0.U, mask = 0.U, addr = 0.U)

        }
      case _: TLBundleD =>
        val bndc = bnd.asInstanceOf[TLBundleD]
        if (bndc.opcode.litValue() == 0) {

          assert(bndc.param.litValue() == 0, "Non-zero param field for ACCESSACK TLBundle")
          assert(!bndc.corrupt.litToBoolean, "Corrupt ACCESSACK TLBundle")
          AccessAck(size = bndc.size, source = bndc.source, denied = bndc.denied)

        } else if (bndc.opcode.litValue() == 1) {

          // Assertions checking on first TLBundle
          assert(bndc.param.litValue() == 0, "Non-zero param field for ACCESSACKDATA TLBundle")
          if (bndc.denied.litToBoolean) {
            assert(bndc.corrupt.litToBoolean, "ACCESSACKDATA denied but not corrupt")
          }

          if (bndsq.size > 0) {
            var datas = new ListBuffer[UInt]
            datas += bndc.data

            while (bndsq.nonEmpty) {
              // Checking if other bundles in burst are valid
              val other_bndc = bndsq.dequeue().asInstanceOf[TLBundleD]

              assert(bndc.param.litValue() == other_bndc.param.litValue())
              assert(bndc.size.litValue() == other_bndc.size.litValue())

              datas += other_bndc.data
            }
            AccessAckDataBurst(size = bndc.size, source = bndc.source, denied = bndc.denied, datas = datas.toList)
          } else {
            AccessAckData(size = bndc.size, source = bndc.source, denied = bndc.denied, data = bndc.data)
          }

        } else if (bndc.opcode.litValue() == 2) {

          assert(bndc.param.litValue() == 0, "Non-zero param field for HINTACK TLBundle")
          assert(!bndc.corrupt.litToBoolean, "Corrupt HINTACK TLBundle")
          HintAck(size = bndc.size, source = bndc.source, denied = bndc.denied)

        } else {

          assert(false, "Invalid OPCODE on D Channel")
          AccessAck(size = 0.U, source = bndc.source, denied = true.B)

        }
    }
  }

  // Helper methods for filtering monitored transactions
  def filterA (txn : TLChannel) : Boolean = {
    txn match {
      case _: TLBundleA => true
      case _ => false
    }
  }

  def filterB (txn : TLChannel) : Boolean = {
    txn match {
      case _: TLBundleB => true
      case _ => false
    }
  }

  def filterC (txn : TLChannel) : Boolean = {
    txn match {
      case _: TLBundleC => true
      case _ => false
    }
  }

  def filterD (txn : TLChannel) : Boolean = {
    txn match {
      case _: TLBundleD => true
      case _ => false
    }
  }

  def filterE (txn : TLChannel) : Boolean = {
    txn match {
      case _: TLBundleE => true
      case _ => false
    }
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

  // State is a byte-addressed HashMap
  def readData(state: HashMap[Int,Int], size: UInt, address: UInt, mask: UInt, beatBytes: Int = 8): List[UInt] = {
    val sizeInt = 1 << size.litValue().toInt
    val addressInt = address.litValue().toInt
    val byteMask = toByteMask(mask)
    var tempResult: BigInt = 0
    var resultList = ListBuffer[UInt]()
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
  def writeData(state : HashMap[Int,Int], size: UInt, address: UInt, datas: List[UInt], masks: List[UInt], beatBytes: Int = 8): Unit = {
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

  // Response function required for TL SDrivers
  def testResponse (input : TLTransaction, state : HashMap[Int,Int]) : (TLTransaction, HashMap[Int,Int]) = {
    val beatBytesSize = 3
    // Default response is corrupt transaction
    var responseTLTxn : TLTransaction = AccessAck(0.U, true.B)
    // Making internal copy of state (non-destructive)
    val state_int = state.clone()

    // Transaction response
    input match {
      case _: Get =>
        val txnc = input.asInstanceOf[Get]

        // Read Data
        val readOut = readData(state = state_int, size = txnc.size, address  = txnc.addr, mask = txnc.mask)

        if (txnc.size.litValue() > beatBytesSize)
          responseTLTxn = AccessAckDataBurst(size = txnc.size, denied = false.B, datas = readOut)
        else
          responseTLTxn = AccessAckData(size = txnc.size, denied = false.B, data = readOut(0))

      case _: PutFull | _: PutPartial | _: PutFullBurst | _: PutPartialBurst =>
        var size  = 0.U
        var address = 0.U
        var datas = List[UInt]()
        var masks  = List[UInt]()

        input match {
          case _: PutFull =>
            val txnc = input.asInstanceOf[PutFull]
            size = beatBytesSize.U
            address = txnc.addr
            datas = List(txnc.data)
            masks = List(txnc.mask)
          case _: PutPartial =>
            val txnc = input.asInstanceOf[PutPartial]
            size = beatBytesSize.U
            address = txnc.addr
            datas = List(txnc.data)
            masks = List(txnc.mask)
          case _: PutFullBurst =>
            val txnc = input.asInstanceOf[PutFullBurst]
            size = txnc.size
            address = txnc.addr
            datas = txnc.datas
            masks = txnc.masks
          case _: PutPartialBurst =>
            val txnc = input.asInstanceOf[PutPartialBurst]
            size = txnc.size
            address = txnc.addr
            datas = txnc.datas
            masks = txnc.masks
        }

        // Write Data
        writeData(state = state_int, size = size, address = address, datas = datas, masks = masks)

        // Response
        responseTLTxn = AccessAck(size = size, denied = false.B)

      case _: ArithData | _: ArithDataBurst | _: LogicData | _: LogicDataBurst =>
        var param = 0.U
        var size  = 0.U
        var address = 0.U
        var datas = List[UInt]()
        var masks  = List[UInt]()
        var burst = false
        var arith = true

        input match {
          case _: ArithData =>
            val txnc = input.asInstanceOf[ArithData]
            param = txnc.param
            size = beatBytesSize.U
            address = txnc.addr
            datas = List(txnc.data)
            masks = List(txnc.mask)

          case _: ArithDataBurst =>
            val txnc = input.asInstanceOf[ArithDataBurst]
            param = txnc.param
            size = txnc.size
            address = txnc.addr
            datas = txnc.datas
            masks = txnc.masks
            burst = true
          case _: LogicData =>
            val txnc = input.asInstanceOf[LogicData]
            param = txnc.param
            size = beatBytesSize.U
            address = txnc.addr
            datas = List(txnc.data)
            masks = List(txnc.mask)
            arith = false

          case _: LogicDataBurst =>
            val txnc = input.asInstanceOf[LogicDataBurst]
            param = txnc.param
            size = txnc.size
            address = txnc.addr
            datas = txnc.datas
            masks = txnc.masks
            burst = true
            arith = false
        }

        var function : (UInt, UInt) => UInt = min
        if (arith) {
          // Arithmetic Function
          param.litValue().toInt match {
            case 0 => function = min
            case 1 => function = max
            case 2 => function = minu
            case 3 => function = maxu
            case 4 => function = add
          }
        } else {
          // Logical Function
          param.litValue().toInt match {
            case 0 => function = xor
            case 1 => function = or
            case 2 => function = and
            case 3 => function = swap
          }
        }

        // Reading Old Data (to return)
        val oldData = readData(state = state_int, size = size, address = address, mask = 0xff.U)

        // Creating newData (to write)
        var newData = ListBuffer[UInt]()
        for (((n, m), o) <- (datas zip masks) zip oldData) {
          newData += function(o, (n.litValue() & toByteMask(m)).U)
        }

        writeData(state = state_int, size = size, address = address, datas = newData.toList, masks = List.fill(newData.length)(0xff.U))

        if (burst) responseTLTxn = AccessAckDataBurst(size = size, denied = false.B, datas = oldData)
        else responseTLTxn = AccessAckData(size = beatBytesSize.U, denied = false.B, data = oldData.head)

      case _: Intent =>
        val txnc = input.asInstanceOf[Intent]

        // Current don't accept hints
        responseTLTxn = HintAck(txnc.size, denied = true.B)
    }

    (responseTLTxn, state_int)
  }
}