package verif

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.ChiselEnum
import freechips.rocketchip.tilelink.{TLBundleParameters}

abstract case class TLTransaction(implicit val params: TLBundleParameters) extends Bundle with Transaction

trait HasSource { this: TLTransaction =>
  val source: UInt = UInt(this.params.sourceBits.W)
}

trait HasAddr { this: TLTransaction =>
  val addr: UInt = UInt(this.params.addressBits.W)
}

trait HasMask { this: TLTransaction =>
  val mask: UInt = UInt((this.params.dataBits/8).W)
}

trait HasFwd { this: TLTransaction =>
  val fwd: Bool = Bool()
}

trait HasSize { this: TLTransaction =>
  val size: UInt = UInt(this.params.sizeBits.W)
}

trait HasData { this: TLTransaction =>
  val data: UInt = UInt(this.params.dataBits.W)
}

// Channel A
// Note (TL-C): Transactions with fwd = True.B are sent via Channel B
// TODO: get rid of implicits by constructing a base class
// TODO: add checks for truncation when converting Scala types to Chisel hardware types
abstract case class TLTransactionA(implicit params: TLBundleParameters) extends TLTransaction with
  HasSource with HasMask with HasAddr

case class Get()(implicit params: TLBundleParameters) extends TLTransactionA with HasSize with HasFwd
object Get {
  def apply(size: Int, source: Int, addr: BigInt, mask: Int, fwd: Boolean)(implicit params: TLBundleParameters): Get = {
    Get().Lit(_.size -> size.U, _.source -> source.U, _.addr -> addr.U, _.mask -> mask.U, _.fwd -> fwd.B)
  }
}

sealed case class Put()(implicit params: TLBundleParameters) extends TLTransactionA with HasData with HasFwd
// TODO: is this hierarchy necessary or can full vs partial be determined from the mask?
//class PutFull()(implicit params: TLBundleParameters) extends Put
//class PutPartial()(implicit params: TLBundleParameters) extends Put
object Put{
  def apply(source: Int, addr: BigInt, mask: Int, data: BigInt, fwd: Boolean)(implicit params: TLBundleParameters): Put = {
    Put().Lit(_.source -> source.U, _.addr -> addr.U, _.mask -> mask.U, _.data -> data.U, _.fwd -> fwd.B)
  }
  //def PutFull(source: Int, addr: BigInt, mask: Int, data: BigInt, fwd: Boolean)(implicit params: TLBundleParameters): PutFull = {
    //Put().Lit(_.source -> source.U, _.addr -> addr.U, _.mask -> mask.U, _.data -> data.U, _.fwd -> fwd.B).asInstanceOf[PutFull]
  //}
  //def PutPartial(source: Int, addr: BigInt, mask: Int, data: BigInt, fwd: Boolean)(implicit params: TLBundleParameters): PutPartial = {
    //PutFull(source, addr, mask, data, fwd).asInstanceOf[PutPartial]
  //}
}

// TODO: PutBurst is not randomizable
// TODO: migrate to the Bundle version of PutBurst with Vecs
// TODO: does size need to explicitly specified, or can it be computed from beats?
// TODO: can this inherit some fields from Put?
// TODO: does there need to be a PutBurstFull vs PutBurstPartial distinction?
/*
case class PutBurst(puts: Seq[Put])(implicit params: TLBundleParameters) extends TLTransaction
object PutBurst {
  def apply(source: Int, addr: BigInt, maskAndData: Seq[(Int, BigInt)], fwd: Boolean)(implicit params: TLBundleParameters): PutBurst = {
    val puts = maskAndData.map{
      case(mask, data) => Put.PutFull(source, addr, mask, data, fwd)
    }
    PutBurst(puts)
  }
}
 */

/*
case class PutBurst(beats: Int)(implicit params: TLBundleParameters) extends TLTransaction {
  val size: UInt = UInt(params.sizeBits.W)
  val source: UInt = UInt(params.sourceBits.W)
  val addr: UInt = UInt(params.addressBits.W)
  val masks: Vec[UInt] = VecInit(Seq.fill(beats)(UInt((params.dataBits/8).W)))
  val datas: Vec[UInt] = VecInit(Seq.fill(beats)(UInt(params.dataBits.W)))
  val fwd: UInt = Bool()
}

object PutBurst {
  def apply(size: Int, source: Int, addr: BigInt, masks: Seq[Int], datas: Seq[BigInt], fwd: Boolean)(implicit params: TLBundleParameters): PutBurst = {
    PutBurst(datas.length).Lit(_.size -> size.U, _.source -> source.U, _.addr -> addr.U,
      _.masks -> VecInit(masks.map(_.U)), _.datas -> VecInit(datas.map(_.U)), _.fwd -> fwd.B)
    // TODO: does this work at all? How does VecInit work with literal binding?
  }
}
*/

abstract case class Atomic()(implicit params: TLBundleParameters) extends TLTransactionA with HasData with HasFwd
class Arithmetic(implicit params: TLBundleParameters) extends Atomic {
  val param: UInt = UInt(ArithmeticParam.getWidth.W) // TODO: this doesn't constrain the possible values of param to (0-4)
}
class Logical(implicit params: TLBundleParameters) extends Atomic {
  val param: UInt = UInt(LogicalParam.getWidth.W) // TODO: this just coincidentally works
}
object ArithmeticParam extends ChiselEnum {
  val MIN, MAX, MINU, MAXU, ADD = Value
}
object LogicalParam extends ChiselEnum {
  val XOR, OR, AND, SWAP = Value
}

object Arithmetic {
  def apply(param: Int, source: Int, addr: BigInt, mask: Int, data: BigInt, fwd: Boolean)(implicit params: TLBundleParameters): Arithmetic = {
    new Arithmetic().Lit(_.param -> ArithmeticParam(param.U), _.source -> source.U, _.addr -> addr.U, _.mask -> mask.U, _.data -> data.U, _.fwd -> fwd.B)
  }
}
object Logical {
  def apply(param: Int, source: Int, addr: BigInt, mask: Int, data: BigInt, fwd: Boolean)(implicit params: TLBundleParameters): Logical = {
    new Logical().Lit(_.param -> LogicalParam(param.U), _.source -> source.U, _.addr -> addr.U, _.mask -> mask.U, _.data -> data.U, _.fwd -> fwd.B)
  }
}

// There shouldn't be a burst type; just use sequences of Arithmetic/Logical and apply the burst behavior on the driver
/*
case class ArithDataBurst(ariths: Seq[ArithData])(implicit params: TLBundleParameters) extends TLTransaction
object ArithDataBurst {
  def apply(param: Int, source: Int, addr: BigInt, maskAndData: Seq[(Int, BigInt)], fwd: Boolean)(implicit params: TLBundleParameters): ArithDataBurst = {
    val ariths = maskAndData.map{
      case(mask, data) =>
        ArithData(param, source, addr, mask, data, fwd)
    }
    ArithDataBurst(ariths)
  }
}
*/

case class Intent()(implicit params: TLBundleParameters) extends TLTransactionA with HasSize with HasFwd {
  val param: UInt = UInt(IntentParam.getWidth.W)
}
object IntentParam extends ChiselEnum {
  val PrefetchRead, PrefetchWrite = Value
}
object Intent {
  def apply(param: Int, size: Int, source: Int, addr: BigInt, mask: Int, data: BigInt, fwd: Boolean)(implicit params: TLBundleParameters): Intent = {
    Intent().Lit(_.param -> IntentParam(param.U), _.size -> size.U, _.source -> source.U, _.addr -> addr.U, _.mask -> mask.U, _.fwd -> fwd.B)
  }
}

abstract case class Acquire()(implicit params: TLBundleParameters) extends TLTransactionA with HasSize {
  val param: UInt = UInt(AcquireParam.getWidth.W)
}
class AcquireBlock()(implicit params: TLBundleParameters) extends Acquire
class AcquirePerm()(implicit params: TLBundleParameters) extends Acquire
object AcquireParam extends ChiselEnum {
  val NtoB, NtoT, BtoT = Value
}
object AcquireBlock {
  def apply(param: Int, size: Int, source: Int, addr: BigInt, mask: Int)(implicit params: TLBundleParameters): AcquireBlock = {
    new AcquireBlock().Lit(_.param -> AcquireParam(param.U), _.size -> size.U, _.source -> source.U, _.addr -> addr.U, _.mask -> mask.U)
  }
}
object AcquirePerm {
  def apply(param: Int, size: Int, source: Int, addr: BigInt, mask: Int)(implicit params: TLBundleParameters): AcquirePerm = {
    new AcquirePerm().Lit(_.param -> AcquireParam(param.U), _.size -> size.U, _.source -> source.U, _.addr -> addr.U, _.mask -> mask.U)
  }
}

// Channel B
// Note (TL-C): See Channel A transactions for forwarded messages
abstract case class TLTransactionB(implicit params: TLBundleParameters) extends TLTransaction with
  HasSource with HasMask with HasAddr with HasSize {
  val param: UInt = UInt(ProbeParam.getWidth.W)
}
class ProbeBlock()(implicit params: TLBundleParameters) extends TLTransactionB
class ProbePerm()(implicit params: TLBundleParameters) extends TLTransactionB
object ProbeParam extends ChiselEnum {
  val toT, toB, toN = Value
}
object ProbeBlock {
  def apply(param: Int, size: Int, source: Int, addr: BigInt, mask: Int)(implicit params: TLBundleParameters): ProbeBlock = {
    new ProbeBlock().Lit(_.param -> ProbeParam(param.U), _.size -> size.U, _.source -> source.U, _.addr -> addr.U, _.mask -> mask.U)
  }
}
object ProbePerm {
  def apply(param: Int, size: Int, source: Int, addr: BigInt, mask: Int)(implicit params: TLBundleParameters): ProbePerm = {
    new ProbePerm().Lit(_.param -> ProbeParam(param.U), _.size -> size.U, _.source -> source.U, _.addr -> addr.U, _.mask -> mask.U)
  }
}

// Channel C
// Note (TL-C): See Channel D transactions for forwarded messages
abstract case class TLTransactionC(implicit params: TLBundleParameters) extends TLTransaction with
  HasSource with HasSize with HasAddr {
  val param: UInt = UInt(ProbeParam.getWidth.W)
}
case class ProbeAck(param: UInt, size: UInt, source: UInt, addr: UInt) extends TLTransaction
case class ProbeAckData(param: UInt, size: UInt, source: UInt, addr: UInt, data: UInt) extends TLTransaction
//case class ProbeAckDataBurst(param: UInt, size: UInt, source: UInt, addr: UInt, datas: List[UInt]) extends TLTransaction
case class Release(param: UInt, size: UInt, source: UInt, addr: UInt) extends TLTransaction
case class ReleaseData(param: UInt, size: UInt, source: UInt, addr: UInt, data: UInt) extends TLTransaction
//case class ReleaseDataBurst(param: UInt, size: UInt, source: UInt, addr: UInt, datas: List[UInt]) extends TLTransaction

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
