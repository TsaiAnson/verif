package verif

import chisel3._
import chisel3.util.isPow2
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import chisel3.experimental.BundleLiterals._

import scala.collection.mutable.{ListBuffer, Queue}
import scala.math.ceil

trait Transaction extends IgnoreSeqInBundle { this: Bundle =>
  override def equals(that: Any): Boolean = {
    var result = this.getClass() == that.getClass()
    if (result) {
      val fields = this.getClass.getDeclaredFields
      for (field <- fields) {
        field.setAccessible(true)
        result &= field.get(this).asInstanceOf[Data].litValue() == field.get(that).asInstanceOf[Data].litValue()
      }

      // Version using getElements
      // Has a strange bug where elements are sometimes out of order, and equivalent objects would return false
      //      that.asInstanceOf[Bundle].getElements.zipWithIndex.foreach { t : (Data, Int) =>
      //        result &= (this.getElements(t._2).litValue() == t._1.litValue())
      //      }
    }
    result
  }

  override def toString(): String = {
    var result = this.className
    if (this.getElements.nonEmpty) {
      result += "("
      this.getElements.foreach {
        t: Any =>
          t match {
            case _: List[Data] =>
              result += "("
              val list = t.asInstanceOf[List[Data]]
              for (d <- list) {
                result += d.litValue().toString()
              }
              result += ")"
            case _: Data =>
              result += t.asInstanceOf[Data].litValue().toString() + ", "
          }
      }
      result = result.slice(0, result.length - 2) + ")"
    }
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

  // Throws assertions if DUT responds with incorrect fields
  // NOTE: Currently only supports TL-UL
  // WILL BE DEPRECATED ONCE SLAVE MODEL HAS BEEN UPDATED
  def TLBundletoTLTransactionOLD(bnd : TLChannel, TLSParam : TLSlaveParameters = standaloneSlaveParams.managers(0) ) : TLTransaction = {
    bnd match {
      case _: TLBundleA =>
        val bndc = bnd.asInstanceOf[TLBundleA]
        if (bndc.opcode.litValue() == 0) {

          assert(TLSParam.supportsPutFull != TransferSizes.none, "Channel does not support PUTFULL requests.")
          assert(bndc.param.litValue() == 0, "Non-zero param field for PUTFULL TLBundle")
          assert(containsLg(TLSParam.supportsPutFull, bndc.size), "Size is outside of valid transfer sizes")
          assert(alignedLg(bndc.address, bndc.size), s"PUTFULL Address (${bndc.address}) is NOT aligned with size (${bndc.size})")
          //          assert(alignedLg(bndc.mask, bndc.size), s"PUTFULL MASK (${bndc.mask}) is not aligned with size (${bndc.size})")
          assert(contiguous(bndc.mask), "PUTFULL MASK is not contiguous")
          PutFull(addr = bndc.address, mask = bndc.mask, data = bndc.data)

        } else if (bndc.opcode.litValue() == 1) {

          assert(TLSParam.supportsPutPartial != TransferSizes.none, "Channel does not support PUTPARTIAL requests.")
          assert(bndc.param.litValue() == 0, "Non-zero param field for PUTPARTIAL TLBundle")
          assert(containsLg(TLSParam.supportsPutPartial, bndc.size), "Size is outside of valid transfer sizes")
          assert(alignedLg(bndc.address, bndc.size), s"PUTPARTIAL Address (${bndc.address}) is NOT aligned with size (${bndc.size})")
          // TODO Check that high bits are aligned
          PutPartial(addr = bndc.address, mask = bndc.mask, data = bndc.data)

        } else if (bndc.opcode.litValue() == 4) {

          assert(TLSParam.supportsGet != TransferSizes.none, "Channel does not support GET requests.")
          assert(bndc.param.litValue() == 0, "Non-zero param field for GET TLBundle")
          assert(containsLg(TLSParam.supportsGet, bndc.size), "Size is outside of valid transfer sizes")
          // Need to check
          //          assert(alignedLg(bndc.mask, bndc.size), "GET MASK is not aligned")
          assert(contiguous(bndc.mask), "GET MASK is not contiguous")
          assert(!bndc.corrupt.litToBoolean, "Corrupt GET TLBundle")
          Get(size = bndc.size, addr = bndc.address, mask = bndc.mask)

        } else {

          assert(false, "Invalid OPCODE on A Channel")
          Get(size = 0.U, addr = 0.U, mask = 0.U)

        }
      case _: TLBundleD =>
        val bndc = bnd.asInstanceOf[TLBundleD]
        if (bndc.opcode.litValue() == 0) {

          assert(bndc.param.litValue() == 0, "Non-zero param field for ACCESSACK TLBundle")
          assert(!bndc.corrupt.litToBoolean, "Corrupt ACCESSACK TLBundle")
          AccessAck(size = bndc.size, denied = bndc.denied)

        } else if (bndc.opcode.litValue() == 1) {

          assert(bndc.param.litValue() == 0, "Non-zero param field for ACCESSACKDATA TLBundle")
          if (bndc.denied.litToBoolean) {
            assert(bndc.corrupt.litToBoolean, "ACCESSACKDATA denied but not corrupt")
          }
          AccessAckData(size = bndc.size, denied = bndc.denied, data = bndc.data)

        } else {

          assert(false, "Invalid OPCODE on D Channel")
          AccessAck(size = 0.U, denied = true.B)

        }
    }
  }

  // WILL BE DEPRECATED ONCE SLAVE MODEL HAS BEEN UPDATED
  def TLTransactiontoTLBundleOLD(txn : TLTransaction) : TLChannel = {
    txn match {
      case _: PutFull =>
        val txnc = txn.asInstanceOf[PutFull]
        TLUBundleAHelper(opcode = 0.U, address = txnc.addr, data = txnc.data)
      case _: PutPartial =>
        val txnc = txn.asInstanceOf[PutPartial]
        TLUBundleAHelper(opcode = 0.U, address = txnc.addr, mask = txnc.mask, data = txnc.data)
      case _: Get =>
        val txnc = txn.asInstanceOf[Get]
        TLUBundleAHelper(opcode = 4.U, address = txnc.addr)
      case _: AccessAck =>
        TLUBundleDHelper()
      case _: AccessAckData =>
        val txnc = txn.asInstanceOf[AccessAckData]
        TLUBundleDHelper(data = txnc.data)
    }
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
  def isNoneBurst (bnd : TLChannel): Boolean = {
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

  // Helper method to group together burst TLBundles
  def groupTLBundles (txns: List[TLChannel]) : List[List[TLChannel]] = {
    // Hardcoded for now, update when configurability is added
    val beatBytes = 8
    val txnsQ = new Queue[TLChannel]()
    txnsQ ++= txns
    var result = new ListBuffer[List[TLChannel]]

    while (txnsQ.nonEmpty) {
      var txnCount = ceil(getTLBundleDataSizeBytes(txnsQ.front) / beatBytes.toDouble).toInt
      // Overriding txnCount to 1 if just AccessAck
      txnCount = if (isNoneBurst(txnsQ.front)) 1 else txnCount
      val newList = new ListBuffer[TLChannel]
      for ( _ <- 0 until txnCount) {
        newList += txnsQ.dequeue()
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
          assert(bndc.param.litValue() > 4, s"Non-valid PARAM (${bndc.param}) for ARITHMETIC Data Bundle")
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
          assert(bndc.param.litValue() > 33, s"Non-valid PARAM (${bndc.param}) for LOGIC Data Bundle")
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
}