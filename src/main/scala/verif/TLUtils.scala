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

// Channel A
// Note (TL-C): Transactions with fwd = True.B are sent via Channel B
case class Get(size: UInt, source: UInt, addr: UInt, mask: UInt, fwd: Bool = false.B) extends TLTransaction
case class PutFull(source: UInt, addr: UInt, mask: UInt, data: UInt, fwd: Bool = false.B) extends TLTransaction
case class PutFullBurst(size: UInt, source: UInt, addr: UInt, masks: List[UInt], datas: List[UInt], fwd: Bool = false.B) extends TLTransaction
case class PutPartial(source: UInt, addr: UInt, mask: UInt, data: UInt, fwd: Bool = false.B) extends TLTransaction
case class PutPartialBurst(size: UInt, source: UInt, addr: UInt, masks: List[UInt], datas: List[UInt], fwd: Bool = false.B) extends TLTransaction
case class ArithData(param: UInt, source: UInt, addr: UInt, mask: UInt, data: UInt, fwd: Bool = false.B) extends TLTransaction
case class ArithDataBurst(param: UInt, size: UInt, source: UInt, addr: UInt, masks: List[UInt], datas: List[UInt], fwd: Bool = false.B) extends TLTransaction
case class LogicData(param: UInt, source: UInt, addr: UInt, mask: UInt, data: UInt, fwd: Bool = false.B) extends TLTransaction
case class LogicDataBurst(param: UInt, size: UInt, source: UInt, addr: UInt, masks: List[UInt], datas: List[UInt], fwd: Bool = false.B) extends TLTransaction
case class Intent(param: UInt, size: UInt, source: UInt, addr: UInt, mask: UInt, fwd: Bool = false.B) extends TLTransaction
case class AcquireBlock(param: UInt, size: UInt, source: UInt, addr: UInt, mask: UInt) extends TLTransaction
case class AcquirePerm(param: UInt, size: UInt, source: UInt, addr: UInt, mask: UInt) extends TLTransaction

// Channel B
// Note (TL-C): See Channel A transactions for forwarded messages
case class ProbeBlock(param: UInt, size: UInt, source: UInt, addr: UInt, mask: UInt) extends TLTransaction
case class ProbePerm(param: UInt, size: UInt, source: UInt, addr: UInt, mask: UInt) extends TLTransaction

// Channel C
// Note (TL-C): See Channel D transactions for forwarded messages
case class ProbeAck(param: UInt, size: UInt, source: UInt, addr: UInt) extends TLTransaction
case class ProbeAckData(param: UInt, size: UInt, source: UInt, addr: UInt, data: UInt) extends TLTransaction
case class ProbeAckDataBurst(param: UInt, size: UInt, source: UInt, addr: UInt, datas: List[UInt]) extends TLTransaction
case class Release(param: UInt, size: UInt, source: UInt, addr: UInt) extends TLTransaction
case class ReleaseData(param: UInt, size: UInt, source: UInt, addr: UInt, data: UInt) extends TLTransaction
case class ReleaseDataBurst(param: UInt, size: UInt, source: UInt, addr: UInt, datas: List[UInt]) extends TLTransaction

// Channel D
// Note (TL-C): Transactions with fwd = True.B are sent via Channel C
case class AccessAck(size: UInt, denied: Bool, source: UInt = 0.U, fwd: Bool = false.B, addr: UInt = 0x0.U(64.W)) extends TLTransaction
case class AccessAckData(size: UInt, denied: Bool, data: UInt, source: UInt = 0.U, fwd: Bool = false.B, addr: UInt = 0x0.U(64.W)) extends TLTransaction
case class AccessAckDataBurst(size: UInt, denied: Bool, datas: List[UInt], source: UInt = 0.U, fwd: Bool = false.B, addr: UInt = 0x0.U(64.W)) extends TLTransaction
case class HintAck(size: UInt, denied: Bool, source: UInt = 0.U, fwd: Bool = false.B, addr: UInt = 0x0.U(64.W)) extends TLTransaction
case class Grant(param: UInt, size: UInt, source: UInt, sink: UInt, denied: Bool) extends TLTransaction
case class GrantData(param: UInt, size: UInt, source: UInt, sink: UInt, denied: Bool, data: UInt) extends TLTransaction
case class GrantDataBurst(param: UInt, size: UInt, source: UInt, sink: UInt, denied: Bool, datas: List[UInt]) extends TLTransaction
case class ReleaseAck(size: UInt, source: UInt) extends TLTransaction

// Channel E
case class GrantAck(sink: UInt) extends TLTransaction

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

  // Temporary cache parameters
  def standaloneSlaveParamsC: TLSlavePortParameters = TLSlavePortParameters.v1(Seq(TLSlaveParameters.v1(address = Seq(AddressSet(0x0, 0xfff)),
    supportsGet = TransferSizes(1, 32), supportsPutFull = TransferSizes(1, 32), supportsPutPartial = TransferSizes(1, 32),
    supportsLogical = TransferSizes(1, 32), supportsArithmetic = TransferSizes(1, 32), supportsHint = TransferSizes(1, 32),
    regionType = RegionType.UNCACHED)),
    endSinkId = 1, beatBytes = 8)
  def standaloneMasterParamsC: TLMasterPortParameters = TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = "TestBundle",
    supportsProbe = TransferSizes(1, 32), supportsGet = TransferSizes(1, 32), supportsPutFull = TransferSizes(1, 32),
    supportsPutPartial = TransferSizes(1, 32), supportsLogical = TransferSizes(1, 32), supportsArithmetic = TransferSizes(1, 32),
    supportsHint = TransferSizes(1, 32))))
  def verifTLBundleParamsC: TLBundleParameters = TLBundleParameters(standaloneMasterParamsC, standaloneSlaveParamsC)


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

  // UPGRADED FROM TLTransactiontoTLBundle
  // TODO Correct MASK assertion checking
  def TLTransactiontoTLBundles(txn: TLTransaction): List[TLChannel] = {
    var result = new ListBuffer[TLChannel]()
    txn match {
      case _: PutFull =>
        val txnc = txn.asInstanceOf[PutFull]
        if (txnc.fwd.litToBoolean) {
          result += TLUBundleBHelper(opcode = 0.U, source = txnc.source, address = txnc.addr, data = txnc.data)
        } else {
          result += TLUBundleAHelper(opcode = 0.U, source = txnc.source, address = txnc.addr, data = txnc.data)
        }
      case _: PutFullBurst =>
        val txnc = txn.asInstanceOf[PutFullBurst]
        if (txnc.fwd.litToBoolean) {
          for ((m, d) <- (txnc.masks zip txnc.datas)) {
            result += TLUBundleBHelper(opcode = 0.U, size = txnc.size, source = txnc.source, address = txnc.addr, mask = m, data = d)
          }
        } else {
          for ((m, d) <- (txnc.masks zip txnc.datas)) {
            result += TLUBundleAHelper(opcode = 0.U, size = txnc.size, source = txnc.source, address = txnc.addr, mask = m, data = d)
          }
        }
      case _: PutPartial =>
        val txnc = txn.asInstanceOf[PutPartial]
        if (txnc.fwd.litToBoolean) {
          result += TLUBundleBHelper(opcode = 0.U, source = txnc.source, address = txnc.addr, mask = txnc.mask, data = txnc.data)
        } else {
          result += TLUBundleAHelper(opcode = 0.U, source = txnc.source, address = txnc.addr, mask = txnc.mask, data = txnc.data)
        }
      case _: PutPartialBurst =>
        val txnc = txn.asInstanceOf[PutPartialBurst]
        if (txnc.fwd.litToBoolean) {
          for ((m, d) <- (txnc.masks zip txnc.datas)) {
            result += TLUBundleBHelper(opcode = 0.U, size = txnc.size, source = txnc.source, address = txnc.addr, mask = m, data = d)
          }
        } else {
          for ((m, d) <- (txnc.masks zip txnc.datas)) {
            result += TLUBundleAHelper(opcode = 0.U, size = txnc.size, source = txnc.source, address = txnc.addr, mask = m, data = d)
          }
        }
      case _: ArithData =>
        val txnc = txn.asInstanceOf[ArithData]
        if (txnc.fwd.litToBoolean) {
          result += TLUBundleBHelper(opcode = 2.U, param = txnc.param, source = txnc.source, address = txnc.addr, mask = txnc.mask, data = txnc.data)
        } else {
          result += TLUBundleAHelper(opcode = 2.U, param = txnc.param, source = txnc.source, address = txnc.addr, mask = txnc.mask, data = txnc.data)
        }
      case _: ArithDataBurst =>
        val txnc = txn.asInstanceOf[ArithDataBurst]
        if (txnc.fwd.litToBoolean) {
          for ((m, d) <- (txnc.masks zip txnc.datas)) {
            result += TLUBundleBHelper(opcode = 2.U, param = txnc.param, size = txnc.size, source = txnc.source, address = txnc.addr, mask = m, data = d)
          }
        } else {
          for ((m, d) <- (txnc.masks zip txnc.datas)) {
            result += TLUBundleAHelper(opcode = 2.U, param = txnc.param, size = txnc.size, source = txnc.source, address = txnc.addr, mask = m, data = d)
          }
        }
      case _: LogicData =>
        val txnc = txn.asInstanceOf[LogicData]
        if (txnc.fwd.litToBoolean) {
          result += TLUBundleBHelper(opcode = 3.U, param = txnc.param, source = txnc.source, address = txnc.addr, mask = txnc.mask, data = txnc.data)
        } else {
          result += TLUBundleAHelper(opcode = 3.U, param = txnc.param, source = txnc.source, address = txnc.addr, mask = txnc.mask, data = txnc.data)
        }
      case _: LogicDataBurst =>
        val txnc = txn.asInstanceOf[LogicDataBurst]
        if (txnc.fwd.litToBoolean) {
          for ((m, d) <- (txnc.masks zip txnc.datas)) {
            result += TLUBundleBHelper(opcode = 3.U, param = txnc.param, size = txnc.size, source = txnc.source, address = txnc.addr, mask = m, data = d)
          }
        } else {
          for ((m, d) <- (txnc.masks zip txnc.datas)) {
            result += TLUBundleAHelper(opcode = 3.U, param = txnc.param, size = txnc.size, source = txnc.source, address = txnc.addr, mask = m, data = d)
          }
        }
      case _: Get =>
        val txnc = txn.asInstanceOf[Get]
        if (txnc.fwd.litToBoolean) {
          result += TLUBundleBHelper(opcode = 4.U, size = txnc.size, source = txnc.source, address = txnc.addr)
        } else {
          result += TLUBundleAHelper(opcode = 4.U, size = txnc.size, source = txnc.source, address = txnc.addr)
        }
      case _: Intent =>
        val txnc = txn.asInstanceOf[Intent]
        if (txnc.fwd.litToBoolean) {
          result += TLUBundleBHelper(opcode = 5.U, param = txnc.param, size = txnc.size, source = txnc.source, address = txnc.addr, mask = txnc.mask)
        } else {
          result += TLUBundleAHelper(opcode = 5.U, param = txnc.param, size = txnc.size, source = txnc.source, address = txnc.addr, mask = txnc.mask)
        }
      case _: AcquireBlock =>
        val txnc = txn.asInstanceOf[AcquireBlock]
        result += TLUBundleAHelper(opcode = 6.U, param = txnc.param, size = txnc.size, source = txnc.source, address = txnc.addr, mask = txnc.mask)
      case _: AcquirePerm =>
        val txnc = txn.asInstanceOf[AcquirePerm]
        result += TLUBundleAHelper(opcode = 7.U, param = txnc.param, size = txnc.size, source = txnc.source, address = txnc.addr, mask = txnc.mask)
      case _: ProbeBlock =>
        val txnc = txn.asInstanceOf[ProbeBlock]
        result += TLUBundleBHelper(opcode = 6.U, param = txnc.param, size = txnc.size, source = txnc.source, address = txnc.addr, mask = txnc.mask)
      case _: ProbePerm =>
        val txnc = txn.asInstanceOf[ProbePerm]
        result += TLUBundleBHelper(opcode = 7.U, param = txnc.param, size = txnc.size, source = txnc.source, address = txnc.addr, mask = txnc.mask)
      case _: ProbeAck =>
        val txnc = txn.asInstanceOf[ProbeAck]
        result += TLUBundleCHelper(opcode = 4.U, param = txnc.param, size = txnc.size, source = txnc.source, address = txnc.addr)
      case _: ProbeAckData =>
        val txnc = txn.asInstanceOf[ProbeAckData]
        result += TLUBundleCHelper(opcode = 5.U, param = txnc.param, size = txnc.size, source = txnc.source, address = txnc.addr, data = txnc.data)
      case _: ProbeAckDataBurst =>
        val txnc = txn.asInstanceOf[ProbeAckDataBurst]
        for (d <- txnc.datas) {
          result += TLUBundleCHelper(opcode = 5.U, param = txnc.param, size = txnc.size, source = txnc.source, address = txnc.addr, data = d)
        }
      case _: Release =>
        val txnc = txn.asInstanceOf[Release]
        result += TLUBundleCHelper(opcode = 6.U, param = txnc.param, size = txnc.param, source = txnc.source, address = txnc.addr)
      case _: ReleaseData =>
        val txnc = txn.asInstanceOf[ReleaseData]
        result += TLUBundleCHelper(opcode = 7.U, param = txnc.param, size = txnc.param, source = txnc.source, address = txnc.addr, data = txnc.data)
      case _: ReleaseDataBurst =>
        val txnc = txn.asInstanceOf[ReleaseDataBurst]
        for (d <- txnc.datas) {
          result += TLUBundleCHelper(opcode = 7.U, param = txnc.param, size = txnc.param, source = txnc.source, address = txnc.addr, data = d)
        }
      case _: AccessAck =>
        val txnc = txn.asInstanceOf[AccessAck]
        if (txnc.fwd.litToBoolean) {
          result += TLUBundleCHelper(size = txnc.size, source = txnc.source, address = txnc.addr)
        } else {
          result += TLUBundleDHelper(size = txnc.size, source = txnc.source, denied = txnc.denied)
        }
      case _: AccessAckData =>
        val txnc = txn.asInstanceOf[AccessAckData]
        if (txnc.fwd.litToBoolean) {
          result += TLUBundleCHelper(opcode = 1.U, size = txnc.size, source = txnc.source, address = txnc.addr, data = txnc.data)
        } else {
          result += TLUBundleDHelper(opcode = 1.U, size = txnc.size, source = txnc.source, denied = txnc.denied, data = txnc.data)
        }
      case _: AccessAckDataBurst =>
        val txnc = txn.asInstanceOf[AccessAckDataBurst]
        if (txnc.fwd.litToBoolean) {
          for (d <- txnc.datas) {
            result += TLUBundleCHelper(opcode = 1.U, size = txnc.size, source = txnc.source, address = txnc.addr, data = d)
          }
        } else {
          for (d <- txnc.datas) {
            result += TLUBundleDHelper(opcode = 1.U, size = txnc.size, source = txnc.source, denied = txnc.denied, data = d)
          }
        }
      case _: HintAck =>
        val txnc = txn.asInstanceOf[HintAck]
        if (txnc.fwd.litToBoolean) {
          result += TLUBundleCHelper(opcode = 2.U, size = txnc.size, source = txnc.source, address = txnc.addr)
        } else {
          result += TLUBundleDHelper(opcode = 2.U, size = txnc.size, source = txnc.source, denied = txnc.denied)
        }
      case _: Grant =>
        val txnc = txn.asInstanceOf[Grant]
        result += TLUBundleDHelper(opcode = 4.U, param = txnc.param, size = txnc.size, source = txnc.source, sink = txnc.sink, denied = txnc.denied)
      case _: GrantData =>
        val txnc = txn.asInstanceOf[GrantData]
        result += TLUBundleDHelper(opcode = 5.U, param = txnc.param, size = txnc.size, source = txnc.source, sink = txnc.sink, denied = txnc.denied, data = txnc.data)
      case _: GrantDataBurst =>
        val txnc = txn.asInstanceOf[GrantDataBurst]
        for (d <- txnc.datas) {
          result += TLUBundleDHelper(opcode = 5.U, param = txnc.param, size = txnc.size, source = txnc.source, sink = txnc.sink, denied = txnc.denied, data = d)
        }
      case _: ReleaseAck =>
        val txnc = txn.asInstanceOf[ReleaseAck]
        result += TLUBundleDHelper(opcode = 6.U, size = txnc.size, source = txnc.source)
      case _: GrantAck =>
        val txnc = txn.asInstanceOf[GrantAck]
        result += TLUBundleEHelper(sink = txnc.sink)
    }
    result.toList
  }

  // Helper method to get size of TLChannel
  def getTLBundleDataSizeBytes (bnd : TLChannel): Int = {
    bnd match {
      case _: TLBundleA =>
        val bndc = bnd.asInstanceOf[TLBundleA]
        1 << bndc.size.litValue().toInt
      case _: TLBundleB =>
        val bndc = bnd.asInstanceOf[TLBundleB]
        1 << bndc.size.litValue().toInt
      case _: TLBundleC =>
        val bndc = bnd.asInstanceOf[TLBundleC]
        1 << bndc.size.litValue().toInt
      case _: TLBundleD =>
        val bndc = bnd.asInstanceOf[TLBundleD]
        1 << bndc.size.litValue().toInt
      case _: TLBundleE =>
        1
    }
  }

  // Helper method to see if transaction is non-Burst
  def isNonBurst (bnd : TLChannel): Boolean = {
    bnd match {
      case _: TLBundleA =>
        val bndc = bnd.asInstanceOf[TLBundleA]
        // Get, AcquireBlock, AcquirePerm
        if (bndc.opcode.litValue() == 4 || bndc.opcode.litValue() == 6 || bndc.opcode.litValue() == 7) {
          return true
        }
        false
      case _: TLBundleB =>
        val bndc = bnd.asInstanceOf[TLBundleB]
        // Get, ProbeBlock, ProbePerm
        if (bndc.opcode.litValue() == 4 || bndc.opcode.litValue() == 6 || bndc.opcode.litValue() == 7) {
          return true
        }
        false
      case _: TLBundleC =>
        val bndc = bnd.asInstanceOf[TLBundleC]
        // AccessAck, ProbeAck, Release
        if (bndc.opcode.litValue() == 0 || bndc.opcode.litValue() == 4 || bndc.opcode.litValue() == 5) {
          return true
        }
        false
      case _: TLBundleD =>
        val bndc = bnd.asInstanceOf[TLBundleD]
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

  // UPGRADED FROM TLBundletoTLTransaction
  // TODO Correct MASK assertion checking
  def TLBundlestoTLTransaction(bnds : List[TLChannel]) : TLTransaction = {
    // Currently Hardcoding Parameters
    val TLSParam = standaloneSlaveParams.managers(0)
    val TLMParam = standaloneMasterParams.clients(0)

    val bndsq = new Queue[TLChannel]()
    bndsq ++= bnds
    val bnd = bndsq.dequeue()

    bnd match {
      case _: TLBundleA =>
        val bndc = bnd.asInstanceOf[TLBundleA]
        if (bndc.opcode.litValue() == 0) {

          // Assertions checking on first TLBundle
          assert(TLSParam.supportsPutFull != TransferSizes.none, "(A) Channel does not support PUTFULL requests.")
          assert(bndc.param.litValue() == 0, "(A) Non-zero param field for PUTFULL TLBundle")
          // Only for TL-UL
          //          assert(containsLg(TLSParam.supportsPutFull, bndc.size), "Size is outside of valid transfer sizes")
          assert(alignedLg(bndc.address, bndc.size), s"(A) PUTFULL Address (${bndc.address}) is NOT aligned with size (${bndc.size})")
          // TODO FIX, MASK IS BYTE BASED
          //          assert(alignedLg(bndc.mask, bndc.size), s"PUTFULL MASK (${bndc.mask}) is not aligned with size (${bndc.size})")
          assert(contiguous(bndc.mask), "(A) PUTFULL MASK is not contiguous")
          assert(!bndc.corrupt.litToBoolean, "(A) Corrupt PUTFULL TLBundle")

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
          assert(TLSParam.supportsPutPartial != TransferSizes.none, "(A) Channel does not support PUTPARTIAL requests.")
          assert(bndc.param.litValue() == 0, "(A) Non-zero param field for PUTPARTIAL TLBundle")
          // Only for TL-UL
          //          assert(containsLg(TLSParam.supportsPutPartial, bndc.size), "Size is outside of valid transfer sizes")
          assert(alignedLg(bndc.address, bndc.size), s"(A) PUTPARTIAL Address (${bndc.address}) is NOT aligned with size (${bndc.size})")
          // TODO Check that high bits are aligned
          assert(!bndc.corrupt.litToBoolean, "(A) Corrupt PUTPARTIAL TLBundle")

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
          assert(TLSParam.supportsArithmetic != TransferSizes.none, "(A) Channel does not support ARITHMETIC requests.")
          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() <= 4, s"(A) Non-valid PARAM (${bndc.param}) for ARITHMETIC Data Bundle")
          assert(alignedLg(bndc.address, bndc.size), s"(A) ARITHMETIC Address (${bndc.address}) is NOT aligned with size (${bndc.size})")
          // TODO Add checks for mask
          assert(!bndc.corrupt.litToBoolean, "(A) Corrupt ARITHMETIC TLBundle")

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
          assert(TLSParam.supportsLogical != TransferSizes.none, "(A) Channel does not support LOGIC requests.")
          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() <= 3, s"(A) Non-valid PARAM (${bndc.param}) for LOGIC Data Bundle")
          assert(alignedLg(bndc.address, bndc.size), s"(A) LOGIC Address (${bndc.address}) is NOT aligned with size (${bndc.size})")
          // TODO Add checks for mask
          assert(!bndc.corrupt.litToBoolean, "(A) Corrupt LOGICAL TLBundle")

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
          assert(TLSParam.supportsGet != TransferSizes.none, "(A) Channel does not support GET requests.")
          assert(bndc.param.litValue() == 0, "(A) Non-zero param field for GET TLBundle")
          // Only for TL-UL
          //          assert(containsLg(TLSParam.supportsGet, bndc.size), "Size is outside of valid transfer sizes")
          // Need to check
          //          assert(alignedLg(bndc.mask, bndc.size), "GET MASK is not aligned")
          assert(contiguous(bndc.mask), "(A) GET MASK is not contiguous")
          assert(!bndc.corrupt.litToBoolean, "(A) Corrupt GET TLBundle")
          Get(size = bndc.size, source = bndc.source, mask = bndc.mask, addr = bndc.address)

        } else if (bndc.opcode.litValue() == 5) {

          assert(TLSParam.supportsHint != TransferSizes.none, "(A) Channel does not support INTENT requests.")
          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() <= 1, s"(A) Non-valid PARAM (${bndc.param}) for INTENT Data Bundle")
          // Need to check
          //          assert(alignedLg(bndc.mask, bndc.size), "GET MASK is not aligned")
          assert(!bndc.corrupt.litToBoolean, "(A) Corrupt INTENT TLBundle")

          Intent(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address, mask = bndc.mask)

        } else if (bndc.opcode.litValue() == 6) {

          assert(TLSParam.supportsAcquireB != TransferSizes.none, "(A) Channel does not support AcquireB requests.")
          assert(TLSParam.supportsAcquireT != TransferSizes.none, "(A) Channel does not support AcquireT requests.")
          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() <= 6, s"(A) Non-valid PARAM (${bndc.param}) for ACQUIREBLOCK Bundle")
          // Need to check
          //          assert(alignedLg(bndc.mask, bndc.size), "GET MASK is not aligned")
          assert(!bndc.corrupt.litToBoolean, "(A) Corrupt ACQUIREBLOCK TLBundle")

          AcquireBlock(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address, mask = bndc.mask)

        } else if (bndc.opcode.litValue() == 7) {

          assert(TLSParam.supportsAcquireB != TransferSizes.none, "(A) Channel does not support AcquireB requests.")
          assert(TLSParam.supportsAcquireT != TransferSizes.none, "(A) Channel does not support AcquireT requests.")
          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() <= 6, s"(A) Non-valid PARAM (${bndc.param}) for ACQUIREPERM Bundle")
          // Need to check
          //          assert(alignedLg(bndc.mask, bndc.size), "GET MASK is not aligned")
          assert(!bndc.corrupt.litToBoolean, "(A) Corrupt ACQUIREPERM TLBundle")

          AcquireBlock(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address, mask = bndc.mask)

        } else {

          assert(false, "(A) Invalid OPCODE on A Channel")
          Get(size = 0.U, source = 0.U, mask = 0.U, addr = 0.U)

        }
      case _: TLBundleB =>
        val bndc = bnd.asInstanceOf[TLBundleB]
        if (bndc.opcode.litValue() == 0) {

          // Assertions checking on first TLBundle
          assert(TLSParam.supportsPutFull != TransferSizes.none, "(B) Channel does not support PUTFULL requests.")
          assert(bndc.param.litValue() == 0, "(B) Non-zero param field for PUTFULL TLBundle")
          // Only for TL-UL
          //          assert(containsLg(TLSParam.supportsPutFull, bndc.size), "Size is outside of valid transfer sizes")
          assert(alignedLg(bndc.address, bndc.size), s"(B) PUTFULL Address (${bndc.address}) is NOT aligned with size (${bndc.size})")
          // TODO FIX, MASK IS BYTE BASED
          //          assert(alignedLg(bndc.mask, bndc.size), s"PUTFULL MASK (${bndc.mask}) is not aligned with size (${bndc.size})")
          assert(contiguous(bndc.mask), "(B) PUTFULL MASK is not contiguous")
          assert(!bndc.corrupt.litToBoolean, "(B) Corrupt PUTFULL TLBundle")

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
            PutFullBurst(size = bndc.size, source = bndc.source, addr = bndc.address, masks = masks.toList, datas = datas.toList, fwd = true.B)
          } else {
            PutFull(source = bndc.source, addr = bndc.address, mask = bndc.mask, data = bndc.data, fwd = true.B)
          }
        } else if (bndc.opcode.litValue() == 1) {

          // Assertions checking on first TLBundle
          assert(TLSParam.supportsPutPartial != TransferSizes.none, "(B) Channel does not support PUTPARTIAL requests.")
          assert(bndc.param.litValue() == 0, "(B) Non-zero param field for PUTPARTIAL TLBundle")
          // Only for TL-UL
          //          assert(containsLg(TLSParam.supportsPutPartial, bndc.size), "Size is outside of valid transfer sizes")
          assert(alignedLg(bndc.address, bndc.size), s"(B) PUTPARTIAL Address (${bndc.address}) is NOT aligned with size (${bndc.size})")
          // TODO Check that high bits are aligned
          assert(!bndc.corrupt.litToBoolean, "(B) Corrupt PUTPARTIAL TLBundle")

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
            PutPartialBurst(size = bndc.size, source = bndc.source, addr = bndc.address, masks = masks.toList, datas = datas.toList, fwd = true.B)
          } else {
            PutPartial(source = bndc.source, addr = bndc.address, mask = bndc.mask, data = bndc.data, fwd = true.B)
          }

        } else if (bndc.opcode.litValue() == 2) {

          // Assertions checking on first TLBundle
          assert(TLSParam.supportsArithmetic != TransferSizes.none, "(B) Channel does not support ARITHMETIC requests.")
          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() <= 4, s"(B) Non-valid PARAM (${bndc.param}) for ARITHMETIC Data Bundle")
          assert(alignedLg(bndc.address, bndc.size), s"(B) ARITHMETIC Address (${bndc.address}) is NOT aligned with size (${bndc.size})")
          // TODO Add checks for mask
          assert(!bndc.corrupt.litToBoolean, "(B) Corrupt ARITHMETIC TLBundle")

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
              datas = datas.toList, fwd = true.B)
          } else {
            ArithData(param = bndc.param, source = bndc.source, addr = bndc.address, mask = bndc.mask, data = bndc.data, fwd = true.B)
          }

        } else if (bndc.opcode.litValue() == 3) {

          // Assertions checking on first TLBundle
          assert(TLSParam.supportsLogical != TransferSizes.none, "(B) Channel does not support LOGIC requests.")
          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() <= 3, s"(B) Non-valid PARAM (${bndc.param}) for LOGIC Data Bundle")
          assert(alignedLg(bndc.address, bndc.size), s"(B) LOGIC Address (${bndc.address}) is NOT aligned with size (${bndc.size})")
          // TODO Add checks for mask
          assert(!bndc.corrupt.litToBoolean, "(B) Corrupt LOGICAL TLBundle")

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
              datas = datas.toList, fwd = true.B)
          } else {
            LogicData(param = bndc.param, source = bndc.source, addr = bndc.address, mask = bndc.mask, data = bndc.data, fwd = true.B)
          }

        } else if (bndc.opcode.litValue() == 4) {

          // Assertions checking on first TLBundle
          assert(TLSParam.supportsGet != TransferSizes.none, "(B) Channel does not support GET requests.")
          assert(bndc.param.litValue() == 0, "(B) Non-zero param field for GET TLBundle")
          // Only for TL-UL
          //          assert(containsLg(TLSParam.supportsGet, bndc.size), "Size is outside of valid transfer sizes")
          // Need to check
          //          assert(alignedLg(bndc.mask, bndc.size), "GET MASK is not aligned")
          assert(contiguous(bndc.mask), "(B) GET MASK is not contiguous")
          assert(!bndc.corrupt.litToBoolean, "(B) Corrupt GET TLBundle")
          Get(size = bndc.size, source = bndc.source, mask = bndc.mask, addr = bndc.address, fwd = true.B)

        } else if (bndc.opcode.litValue() == 5) {

          // Assertions checking on first TLBundle
          assert(TLSParam.supportsHint != TransferSizes.none, "(B) Channel does not support INTENT requests.")
          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() <= 1, s"(B) Non-valid PARAM (${bndc.param}) for INTENT Data Bundle")
          // Need to check
          //          assert(alignedLg(bndc.mask, bndc.size), "GET MASK is not aligned")
          assert(!bndc.corrupt.litToBoolean, "(B) Corrupt INTENT TLBundle")

          Intent(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address, mask = bndc.mask, fwd = true.B)

        } else if (bndc.opcode.litValue() == 6) {

          // Assertions checking on first TLBundle
          assert(TLMParam.supportsProbe != TransferSizes.none, "(B) Channel does not support PROBEBLOCK requests.")
          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() <= 6, s"(B) Non-valid PARAM (${bndc.param}) for PROBEBLOCK Bundle")
          // Need to check
          //          assert(alignedLg(bndc.mask, bndc.size), "GET MASK is not aligned")
          assert(!bndc.corrupt.litToBoolean, "(B) Corrupt PROBEBLOCK TLBundle")

          ProbeBlock(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address, mask = bndc.mask)

        } else if (bndc.opcode.litValue() == 7) {

          // Assertions checking on first TLBundle
          assert(TLMParam.supportsProbe != TransferSizes.none, "(B) Channel does not support PROBEPERM requests.")
          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() <= 6, s"(B) Non-valid PARAM (${bndc.param}) for PROBEPERM Bundle")
          // Need to check
          //          assert(alignedLg(bndc.mask, bndc.size), "GET MASK is not aligned")
          assert(!bndc.corrupt.litToBoolean, "(B) Corrupt PROBEPERM TLBundle")

          Intent(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address, mask = bndc.mask)

        } else {

          assert(false, "(B) Invalid OPCODE on B Channel")
          Get(size = 0.U, source = 0.U, mask = 0.U, addr = 0.U)

        }
      case _: TLBundleC =>
        val bndc = bnd.asInstanceOf[TLBundleC]
        if (bndc.opcode.litValue() == 0) {

          assert(bndc.param.litValue() == 0, "(C) Non-zero param field for ACCESSACK TLBundle")
          assert(alignedLg(bndc.address, bndc.size), s"(B) ACCESSACK Address (${bndc.address}) is NOT aligned with size (${bndc.size})")
          assert(!bndc.corrupt.litToBoolean, "(C) Corrupt ACCESSACK TLBundle")
          AccessAck(size = bndc.size, source = bndc.source, addr = bndc.address, denied = false.B, fwd = true.B)

        } else if (bndc.opcode.litValue() == 1) {

          // Assertions checking on first TLBundle
          assert(bndc.param.litValue() == 0, "(C) Non-zero param field for ACCESSACKDATA TLBundle")
          assert(alignedLg(bndc.address, bndc.size), s"(B) ACCESSACKData Address (${bndc.address}) is NOT aligned with size (${bndc.size})")

          if (bndsq.size > 0) {
            var datas = new ListBuffer[UInt]
            datas += bndc.data

            while (bndsq.nonEmpty) {
              // Checking if other bundles in burst are valid
              val other_bndc = bndsq.dequeue().asInstanceOf[TLBundleC]

              assert(bndc.param.litValue() == other_bndc.param.litValue())
              assert(bndc.size.litValue() == other_bndc.size.litValue())
              assert(bndc.address.litValue() == other_bndc.address.litValue())

              datas += other_bndc.data
            }
            AccessAckDataBurst(size = bndc.size, source = bndc.source, denied = false.B, addr = bndc.address, datas = datas.toList, fwd = true.B)
          } else {
            AccessAckData(size = bndc.size, source = bndc.source, denied = false.B, addr = bndc.address, data = bndc.data, fwd = true.B)
          }

        } else if (bndc.opcode.litValue() == 2) {

          assert(bndc.param.litValue() == 0, "(C) Non-zero param field for HINTACK TLBundle")
          assert(!bndc.corrupt.litToBoolean, "(C) Corrupt HINTACK TLBundle")
          HintAck(size = bndc.size, source = bndc.source, addr = bndc.address, denied = false.B, fwd = true.B)

        } else if (bndc.opcode.litValue() == 4) {

          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() <= 6, s"(B) Non-valid PARAM (${bndc.param}) for PROBEACK Bundle")
          assert(!bndc.corrupt.litToBoolean, "(C) Corrupt PROBEACK TLBundle")
          ProbeAck(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address)

        } else if (bndc.opcode.litValue() == 5) {

          // Assertions checking on first TLBundle
          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() <= 6, s"(B) Non-valid PARAM (${bndc.param}) for PROBEACKDATA Bundle")
          assert(alignedLg(bndc.address, bndc.size), s"(B) PROBEACKDATA Address (${bndc.address}) is NOT aligned with size (${bndc.size})")

          if (bndsq.size > 0) {
            var datas = new ListBuffer[UInt]
            datas += bndc.data

            while (bndsq.nonEmpty) {
              // Checking if other bundles in burst are valid
              val other_bndc = bndsq.dequeue().asInstanceOf[TLBundleC]

              assert(bndc.param.litValue() == other_bndc.param.litValue())
              assert(bndc.size.litValue() == other_bndc.size.litValue())
              assert(bndc.address.litValue() == other_bndc.address.litValue())

              datas += other_bndc.data
            }
            ProbeAckDataBurst(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address, datas = datas.toList)
          } else {
            ProbeAckData(param = bndc.param, size = bndc.size, source = bndc.source,addr = bndc.address, data = bndc.data)
          }

        } else if (bndc.opcode.litValue() == 6) {

          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() <= 6, s"(B) Non-valid PARAM (${bndc.param}) for RELEASE Bundle")
          assert(!bndc.corrupt.litToBoolean, "(C) Corrupt RELEASE TLBundle")
          Release(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address)

        } else if (bndc.opcode.litValue() == 7) {

          // Assertions checking on first TLBundle
          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() <= 6, s"(B) Non-valid PARAM (${bndc.param}) for RELEASEDATA Bundle")
          assert(alignedLg(bndc.address, bndc.size), s"(B) RELEASEDATA Address (${bndc.address}) is NOT aligned with size (${bndc.size})")

          if (bndsq.size > 0) {
            var datas = new ListBuffer[UInt]
            datas += bndc.data

            while (bndsq.nonEmpty) {
              // Checking if other bundles in burst are valid
              val other_bndc = bndsq.dequeue().asInstanceOf[TLBundleC]

              assert(bndc.param.litValue() == other_bndc.param.litValue())
              assert(bndc.size.litValue() == other_bndc.size.litValue())
              assert(bndc.address.litValue() == other_bndc.address.litValue())

              datas += other_bndc.data
            }
            ReleaseDataBurst(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address, datas = datas.toList)
          } else {
            ReleaseData(param = bndc.param, size = bndc.size, source = bndc.source,addr = bndc.address, data = bndc.data)
          }

        } else {

          assert(false, "(C) Invalid OPCODE on C Channel")
          AccessAck(size = 0.U, source = bndc.source, denied = true.B, fwd = true.B)

        }
      case _: TLBundleD =>
        val bndc = bnd.asInstanceOf[TLBundleD]
        if (bndc.opcode.litValue() == 0) {

          assert(bndc.param.litValue() == 0, "(D) Non-zero param field for ACCESSACK TLBundle")
          assert(!bndc.corrupt.litToBoolean, "(D) Corrupt ACCESSACK TLBundle")
          AccessAck(size = bndc.size, source = bndc.source, denied = bndc.denied)

        } else if (bndc.opcode.litValue() == 1) {

          // Assertions checking on first TLBundle
          assert(bndc.param.litValue() == 0, "(D) Non-zero param field for ACCESSACKDATA TLBundle")
          if (bndc.denied.litToBoolean) {
            assert(bndc.corrupt.litToBoolean, "(D) ACCESSACKDATA denied but not corrupt")
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

          assert(bndc.param.litValue() == 0, "(D) Non-zero param field for HINTACK TLBundle")
          assert(!bndc.corrupt.litToBoolean, "(D) Corrupt HINTACK TLBundle")
          HintAck(size = bndc.size, source = bndc.source, denied = bndc.denied)

        } else if (bndc.opcode.litValue() == 4) {

          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() <= 6, "(D) Non-valid param field for GRANT TLBundle")
          assert(!bndc.corrupt.litToBoolean, "(D) Corrupt GRANT TLBundle")
          Grant(param = bndc.param, size = bndc.size, source = bndc.source, sink = bndc.sink, denied = bndc.denied)

        } else if (bndc.opcode.litValue() == 5) {

          // Assertions checking on first TLBundle
          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() <= 6, "(D) Non-valid param field for GRANTDATA TLBundle")
          if (bndc.denied.litToBoolean) {
            assert(bndc.corrupt.litToBoolean, "(D) GRANTDATA denied but not corrupt")
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
            GrantDataBurst(param = bndc.param, size = bndc.size, source = bndc.source, sink = bndc.sink, denied = bndc.denied, datas = datas.toList)
          } else {
            GrantData(param = bndc.param, size = bndc.size, source = bndc.source, sink = bndc.sink, denied = bndc.denied, data = bndc.data)
          }

        } else if (bndc.opcode.litValue() == 6) {

          assert(bndc.param.litValue() == 0, "(D) Non-zer0 param field for RELEASEACK TLBundle")
          assert(!bndc.corrupt.litToBoolean, "(D) Corrupt RELEASEACK TLBundle")
          assert(!bndc.denied.litToBoolean, "(D) RELEASEACK cannot be denied.")

          ReleaseAck(size = bndc.size, source = bndc.source)

        } else {

          assert(false, "(D) Invalid OPCODE on D Channel")
          AccessAck(size = 0.U, source = bndc.source, denied = true.B)

        }
      case _: TLBundleE =>
        val bndc = bnd.asInstanceOf[TLBundleE]
        // No asserts
        GrantAck(sink = bndc.sink)
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