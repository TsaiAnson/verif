package verif

import chisel3._
import chisel3.experimental.BundleLiterals._
import freechips.rocketchip.tilelink.TLBundleParameters

sealed trait TLTransaction extends Bundle with Transaction

// TODO: more base class refactoring (note nearly all bundles have source, addr, mask)

// Channel A
// Note (TL-C): Transactions with fwd = True.B are sent via Channel B
//case class Get(size: UInt, source: UInt, addr: UInt, mask: UInt, fwd: Bool = false.B) extends TLTransaction
//case class PutFull(source: UInt, addr: UInt, mask: UInt, data: UInt, fwd: Bool = false.B) extends TLTransaction
//case class PutFullBurst(size: UInt, source: UInt, addr: UInt, masks: List[UInt], datas: List[UInt], fwd: Bool = false.B) extends TLTransaction
//case class PutPartial(source: UInt, addr: UInt, mask: UInt, data: UInt, fwd: Bool = false.B) extends TLTransaction
//case class PutPartialBurst(size: UInt, source: UInt, addr: UInt, masks: List[UInt], datas: List[UInt], fwd: Bool = false.B) extends TLTransaction
case class ArithData(param: UInt, source: UInt, addr: UInt, mask: UInt, data: UInt, fwd: Bool = false.B) extends TLTransaction
case class ArithDataBurst(param: UInt, size: UInt, source: UInt, addr: UInt, masks: List[UInt], datas: List[UInt], fwd: Bool = false.B) extends TLTransaction
case class LogicData(param: UInt, source: UInt, addr: UInt, mask: UInt, data: UInt, fwd: Bool = false.B) extends TLTransaction
case class LogicDataBurst(param: UInt, size: UInt, source: UInt, addr: UInt, masks: List[UInt], datas: List[UInt], fwd: Bool = false.B) extends TLTransaction
case class Intent(param: UInt, size: UInt, source: UInt, addr: UInt, mask: UInt, fwd: Bool = false.B) extends TLTransaction
case class AcquireBlock(param: UInt, size: UInt, source: UInt, addr: UInt, mask: UInt) extends TLTransaction
case class AcquirePerm(param: UInt, size: UInt, source: UInt, addr: UInt, mask: UInt) extends TLTransaction

// TODO: get rid of implicits by constructing a base class
// TODO: add checks for truncation when converting Scala types to Chisel hardware types
case class Get()(implicit params: TLBundleParameters) extends TLTransaction {
  val size: UInt = UInt(params.sizeBits.W)
  val source: UInt = UInt(params.sourceBits.W)
  val addr: UInt = UInt(params.addressBits.W)
  val mask: UInt = UInt((params.dataBits/8).W)
  val fwd: UInt = Bool()
}
object Get {
  def apply(size: Int, source: Int, addr: BigInt, mask: Int, fwd: Boolean)(implicit params: TLBundleParameters): Get = {
    Get().Lit(_.size -> size.U, _.source -> source.U, _.addr -> addr.U, _.mask -> mask.U, _.fwd -> fwd.B)
  }
}

sealed case class Put()(implicit params: TLBundleParameters) extends TLTransaction {
  val source: UInt = UInt(params.sourceBits.W)
  val addr: UInt = UInt(params.addressBits.W)
  val mask: UInt = UInt((params.dataBits/8).W)
  val data: UInt = UInt(params.dataBits.W)
  val fwd: UInt = Bool()
}
// TODO: is this hierarchy necessary or can full vs partial be determined from the mask?
case class PutFull()(implicit params: TLBundleParameters) extends Put
case class PutPartial()(implicit params: TLBundleParameters) extends Put
object Put{
  def PutFull(source: Int, addr: BigInt, mask: Int, data: BigInt, fwd: Boolean)(implicit params: TLBundleParameters): Put = {
    Put().Lit(_.source -> source.U, _.addr -> addr.U, _.mask -> mask.U, _.data -> data.U, _.fwd -> fwd.B).asInstanceOf[PutFull]
  }
  def PutPartial(source: Int, addr: BigInt, mask: Int, data: BigInt, fwd: Boolean)(implicit params: TLBundleParameters): Put = {
    PutFull(source, addr, mask, data, fwd).asInstanceOf[PutPartial]
  }
}

// TODO: does size need to explicitly specified, or can it be computed from beats?
// TODO: can this inherit some fields from Put?
// TODO: does there need to be a PutBurstFull vs PutBurstPartial distinction?
case class PutBurst(beats: Int)(implicit params: TLBundleParameters) extends TLTransaction {
  val size: UInt = UInt(params.sizeBits.W)
  val source: UInt = UInt(params.sourceBits.W)
  val addr: UInt = UInt(params.addressBits.W)
  val masks: Vec[UInt] = VecInit(Seq.fill(beats)(UInt((params.dataBits/8).W)))
  val datas: Vec[UInt] = VecInit(Seq.fill(beats)(UInt(params.dataBits.W)))
  val fwd: UInt = Bool()
}
object PutFullBurst {
  def apply(size: Int, source: Int, addr: BigInt, masks: Seq[Int], datas: Seq[BigInt], fwd: Boolean)(implicit params: TLBundleParameters): PutBurst = {
    PutBurst(datas.length).Lit(_.size -> size.U, _.source -> source.U, _.addr -> addr.U,
      _.masks -> VecInit(masks.map(_.U)), _.datas -> VecInit(datas.map(_.U)), _.fwd -> fwd.B)
    // TODO: does this work at all? How does VecInit work with literal binding?
  }
}


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
// Note (TL-C): Transactions with fwd = True.B are sent via Channel C. Channel C requires address field (which D does not have)
case class AccessAck(size: UInt, source : UInt, denied: Bool, fwd: Bool = false.B, addr: UInt = 0x0.U(64.W)) extends TLTransaction
case class AccessAckData(size: UInt, source : UInt, denied: Bool, data: UInt, fwd: Bool = false.B, addr: UInt = 0x0.U(64.W)) extends TLTransaction
case class AccessAckDataBurst(size: UInt, source : UInt, denied: Bool, datas: List[UInt], fwd: Bool = false.B, addr: UInt = 0x0.U(64.W)) extends TLTransaction
case class HintAck(size: UInt, source : UInt, denied: Bool, fwd: Bool = false.B, addr: UInt = 0x0.U(64.W)) extends TLTransaction
case class Grant(param: UInt, size: UInt, source: UInt, sink: UInt, denied: Bool) extends TLTransaction
case class GrantData(param: UInt, size: UInt, source: UInt, sink: UInt, denied: Bool, data: UInt) extends TLTransaction
case class GrantDataBurst(param: UInt, size: UInt, source: UInt, sink: UInt, denied: Bool, datas: List[UInt]) extends TLTransaction
case class ReleaseAck(size: UInt, source: UInt) extends TLTransaction

// Channel E
case class GrantAck(sink: UInt) extends TLTransaction
