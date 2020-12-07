package verif

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chiseltest._
import firrtl.FirrtlProtos.Firrtl.Statement.Register
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, SimpleDevice, TransferSizes}
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.tilelink._
import chisel3.experimental.BundleLiterals._
import chisel3.util.isPow2

import VerifTLUtils._

import scala.math.ceil
import scala.collection.mutable
import scala.collection.mutable.{Queue,ListBuffer}

trait Transaction extends IgnoreSeqInBundle { this: Bundle =>
  override def equals(that: Any): Boolean = {
    var result = this.getClass() == that.getClass()
    if (result) {
      that.asInstanceOf[Bundle].getElements.zipWithIndex.foreach { t : (Data, Int) =>
        result &= (this.getElements(t._2).litValue() == t._1.litValue())
      }
    }
    result
  }

  override def toString(): String = {
    var result = this.className
    if (this.getElements.size > 0) {
      result += "("
      this.getElements.foreach {
        t: Any =>
          t match {
            case _: List[UInt] =>
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

package object VerifTLUtils {
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

  // Helper method to group together burst TLBundles
  def groupTLBundles (txns: List[TLChannel]) : List[List[TLChannel]] = {
    // Hardcoded for now, update when configurability is added
    val beatBytes = 8
    val txnsQ = new Queue[TLChannel]()
    txnsQ ++= txns
    var result = new ListBuffer[List[TLChannel]]

    while (txnsQ.nonEmpty) {
      val txnCount = ceil(getTLBundleDataSizeBytes(txnsQ.front) / beatBytes.toDouble).toInt
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
}

// Functions for TL Master VIP
// Currently supports TL-UL, (TL-UH)
trait VerifTLMasterFunctions {
  def clk: Clock
  def TLChannels: TLBundle


  def pokeA(a: TLBundleA): Unit = {
    val aC = TLChannels.a
    aC.bits.opcode.poke(a.opcode)
    aC.bits.param.poke(a.param)
    aC.bits.size.poke(a.size)
    aC.bits.source.poke(a.source)
    aC.bits.address.poke(a.address)
    aC.bits.mask.poke(a.mask)
    aC.bits.data.poke(a.data)
  }

  def peekB(): TLBundleB = {
    val bC = TLChannels.b
    val opcode = bC.bits.opcode.peek()
    val param = bC.bits.param.peek()
    val size = bC.bits.size.peek()
    val source = bC.bits.source.peek()
    val address = bC.bits.address.peek()
    val mask = bC.bits.mask.peek()
    val data = bC.bits.data.peek()

    TLUBundleBHelper(opcode, param, size, source, address, mask, data)
  }

  def pokeC(c: TLBundleC): Unit = {
    val cC = TLChannels.c
    cC.bits.opcode.poke(c.opcode)
    cC.bits.param.poke(c.param)
    cC.bits.size.poke(c.size)
    cC.bits.source.poke(c.source)
    cC.bits.address.poke(c.address)
    cC.bits.data.poke(c.data)
    cC.bits.corrupt.poke(c.corrupt)
  }

  def peekD(): TLBundleD = {
    val dC = TLChannels.d
    val opcode = dC.bits.opcode.peek()
    val param = dC.bits.param.peek()
    val size = dC.bits.size.peek()
    val source = dC.bits.source.peek()
    val sink = dC.bits.sink.peek()
    val data = dC.bits.data.peek()
    val corrupt = dC.bits.corrupt.peek()

    TLUBundleDHelper(opcode, param, size, source, sink, data, corrupt)
  }

  def pokeE(e: TLBundleE): Unit = {
    val eC = TLChannels.e
    eC.bits.sink.poke(e.sink)
  }

  def writeA(a: TLBundleA): Unit = {
    val aC = TLChannels.a

    aC.valid.poke(true.B)
    pokeA(a)

    while(!aC.ready.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)

    aC.valid.poke(false.B)
  }

  def readB(): TLBundleB = {
    val bC = TLChannels.b

    bC.ready.poke(true.B)

    while(!bC.valid.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)
    bC.ready.poke(false.B)

    peekB()
  }

  def writeC(c: TLBundleC): Unit = {
    val cC = TLChannels.c

    cC.valid.poke(true.B)
    pokeC(c)

    while(!cC.ready.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)

    cC.valid.poke(false.B)
  }

  def readD(): TLBundleD = {
    val dC = TLChannels.d

    while(!dC.valid.peek().litToBoolean) {
      clk.step(1)
    }

    peekD()
  }

  def writeE(e: TLBundleE): Unit = {
    val eC = TLChannels.e

    eC.valid.poke(true.B)
    pokeE(e)

    while(!eC.ready.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)

    eC.valid.poke(false.B)
  }

  // TODO Figure out why poking C and E does not work
  def reset(): Unit = {
    pokeA(TLUBundleAHelper())
//    pokeC(TLUBundleCHelper())
//    pokeE(TLUBundleEHelper())
    TLChannels.a.valid.poke(false.B)
    TLChannels.b.ready.poke(false.B)
    TLChannels.c.valid.poke(false.B)
    TLChannels.d.ready.poke(false.B)
    TLChannels.e.valid.poke(false.B)
  }
}

// Functions for TL Slave VIP
// Currently supports TL-UL, (TL-UH)
trait VerifTLSlaveFunctions {
  def clk: Clock
  def TLChannels: TLBundle

  def peekA(): TLBundleA = {
    val aC = TLChannels.a
    val opcode = aC.bits.opcode.peek()
    val param = aC.bits.param.peek()
    val size = aC.bits.size.peek()
    val source = aC.bits.source.peek()
    val address = aC.bits.address.peek()
    val mask = aC.bits.mask.peek()
    val data = aC.bits.data.peek()

    TLUBundleAHelper(opcode, param, size, source, address, mask, data)
  }

  def pokeB(b: TLBundleB): Unit = {
    val bC = TLChannels.b
    bC.bits.opcode.poke(b.opcode)
    bC.bits.param.poke(b.param)
    bC.bits.size.poke(b.size)
    bC.bits.source.poke(b.source)
    bC.bits.address.poke(b.address)
    bC.bits.mask.poke(b.mask)
    bC.bits.data.poke(b.data)
  }

  def peekC(): TLBundleC = {
    val cC = TLChannels.c
    val opcode = cC.bits.opcode.peek()
    val param = cC.bits.param.peek()
    val size = cC.bits.size.peek()
    val source = cC.bits.source.peek()
    val address = cC.bits.address.peek()
    val data = cC.bits.data.peek()
    val corrupt = cC.bits.corrupt.peek()

    TLUBundleCHelper(opcode, param, size, source, address, data, corrupt)
  }

  def pokeD(d: TLBundleD): Unit = {
    val dC = TLChannels.d
    dC.bits.opcode.poke(d.opcode)
    dC.bits.param.poke(d.param)
    dC.bits.size.poke(d.size)
    dC.bits.source.poke(d.source)
    dC.bits.sink.poke(d.sink)
    dC.bits.data.poke(d.data)
    dC.bits.corrupt.poke(d.corrupt)
  }

  def peekE(): TLBundleE = {
    val eC = TLChannels.e
    val sink = eC.bits.sink.peek()

    TLUBundleEHelper(sink)
  }

  def readA(): TLBundleA = {
    val aC = TLChannels.a

    aC.ready.poke(true.B)

    while(!aC.valid.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)
    aC.ready.poke(false.B)

    peekA()
  }

  def writeB(b: TLBundleB): Unit = {
    val bC = TLChannels.b

    bC.valid.poke(true.B)
    pokeB(b)

    while(!bC.ready.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)

    bC.valid.poke(false.B)
  }

  def readC(): TLBundleC = {
    val cC = TLChannels.c

    cC.ready.poke(true.B)

    while(!cC.valid.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)
    cC.ready.poke(false.B)

    peekC()
  }

  def writeD(d: TLBundleD): Unit = {
    val dC = TLChannels.d

    dC.valid.poke(true.B)
    pokeD(d)

    while(!dC.ready.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)

    dC.valid.poke(false.B)
  }

  def readE(): TLBundleE = {
    val eC = TLChannels.e

    eC.ready.poke(true.B)

    while(!eC.valid.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)
    eC.ready.poke(false.B)

    peekE()
  }

  // TODO Figure out why pokingB doesn't work
  def reset(): Unit = {
//    pokeB(TLUBundleBHelper())
    pokeD(TLUBundleDHelper())
    TLChannels.a.ready.poke(false.B)
//    TLChannels.b.valid.poke(false.B)
//    TLChannels.c.ready.poke(false.B)
    TLChannels.d.valid.poke(false.B)
//    TLChannels.e.ready.poke(false.B)
  }

  def process(req: TLBundleA): Unit
}

// TLDriver acting as a Master node
class TLDriverMaster(clock: Clock, interface: TLBundle) extends VerifTLMasterFunctions {
  val clk = clock
  val TLChannels = interface

  val inputTransactions = Queue[TLChannel]()

  def push(tx: Seq[TLTransaction]): Unit = {
    for (t <- tx) {
      inputTransactions ++= TLTransactiontoTLBundles(t)
    }
  }

  fork {
    // Ready always high (TL monitor always receiving in transactions)
    interface.d.ready.poke(true.B)
    while (true) {
      if (!inputTransactions.isEmpty) {
        val t = inputTransactions.dequeue()
        writeA(t.asInstanceOf[TLBundleA])
        clock.step()
      } else {
        clock.step()
      }
    }
  }
}

// TLMonitor acting as a Master node
class TLMonitorMaster(clock: Clock, interface: TLBundle) extends VerifTLMasterFunctions {
  val clk = clock
  val TLChannels = interface

  val txns = Queue[TLChannel]()

  def getMonitoredTransactions: List[TLTransaction] = {
    val result = new ListBuffer[TLTransaction]
    for (g <- groupTLBundles(txns.toList)) {
      result += TLBundlestoTLTransaction(g)
    }
    result.toList
  }

  def clearMonitoredTransactions(): Unit = {
    txns.clear()
  }

  fork.withRegion(Monitor) {
    // Reads everything
    while (true) {
      txns += readD()
      clock.step()
    }
  }
}

// TLDriver acting as a Slave node
// TODO Currently drives DChannel (results)
class TLDriverSlave(clock: Clock, interface: TLBundle) extends VerifTLSlaveFunctions {
  // Acting like "regmap"
  // TODO: Add byte-level addressing
  var hash = mutable.HashMap(0 -> 10, 0x08 -> 11, 0x10 -> 12, 0x18 -> 13)

  val clk = clock

  val TLChannels = interface

  val txns = Queue[TLTransaction]()

  def getMonitoredTransactions: mutable.MutableList[TLTransaction] = {
//    for (x <- hash.keys) {
//      print(s"(${x}, ${hash(x)}), ")
//    }
//    println("")
    txns
  }

  // Process function currently only takes opcode 0 (FullPut) and 4 (Get)
  // TODO: Implement PartialPut requests
  def process(a : TLBundleA) : Unit = {
    txns += TLBundletoTLTransactionOLD(a)

    if (!(a.opcode.litValue() == 4 || a.opcode.litValue() == 0)) {
      println(s"ONLY FULL-PUT (0) AND GET (4) OPCODE IS PERMITTED. GIVEN OP: ${a.opcode} EXAMPLE TEST.")
    }

    var result = 0.U
    var opcode = 0.U
    var corrupt = false.B
    if (a.opcode.litValue() == 0) {
      hash(a.address.litValue().toInt) = a.data.litValue().toInt
      result = a.data
    } else if (a.opcode.litValue() == 4) {
      if (hash.contains(a.address.litValue().toInt)) {
        result = hash(a.address.litValue().toInt).U
      } else {
        result = 0.U
      }
      opcode = 1.U
    } else {
      assert(false, s"Unknown opcode: ${a.opcode.litValue()}")
      // Marking corrupt as indicator
      corrupt = true.B
    }

    writeD(TLUBundleDHelper(opcode, a.param, a.size, a.source, 0.U, result, corrupt))
  }

  // Currently just processes the requests from master
  fork {
    reset()
    while (true) {
      process(readA())
      clk.step()
    }
  }
}

// TLMonitor acting as a Slave node
class TLMonitorSlave(clock: Clock, interface: TLBundle) extends VerifTLSlaveFunctions {
  val clk = clock
  val TLChannels = interface

  val txns = Queue[TLTransaction]()

  def process(a: TLBundleA) : Unit = {
    txns += TLBundletoTLTransactionOLD(a)
  }

  def getMonitoredTransactions: mutable.MutableList[TLTransaction] = {
    txns
  }

  fork {
    // Reads all requests
    while (true) {
      process(readA())
      clock.step()
    }
  }
}
