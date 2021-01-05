package verif

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.ChiselEnum
import freechips.rocketchip.tilelink.{TLBundleParameters}

abstract case class TLTransaction()(implicit val params: TLBundleParameters) extends Bundle with Transaction

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

trait HasDenied { this: TLTransaction =>
  val denied: Bool = Bool()
}

trait HasSink { this: TLTransaction =>
  val sink: UInt = UInt(this.params.sinkBits.W)
}

// Channel A
// Note (TL-C): Transactions with fwd = True.B are sent via Channel B
// TODO: get rid of implicits by constructing a base class
// TODO: add checks for truncation when converting Scala types to Chisel hardware types
// TODO: reorganize parameter order and add sensible defaults
abstract class TLTransactionA(implicit params: TLBundleParameters) extends TLTransaction with
  HasSource with HasMask with HasAddr

class Get(implicit params: TLBundleParameters) extends TLTransactionA with HasSize with HasFwd
object Get {
  def apply(size: Int, source: Int, addr: BigInt, mask: Int, fwd: Boolean)(implicit params: TLBundleParameters): Get = {
    new Get().Lit(_.size -> size.U, _.source -> source.U, _.addr -> addr.U, _.mask -> mask.U, _.fwd -> fwd.B)
  }
}

class Put(implicit params: TLBundleParameters) extends TLTransactionA with HasData with HasFwd
object Put{
  // TODO: create a another constructor without mask (implicitly PutFull)
  def apply(source: Int, addr: BigInt, mask: Int, data: BigInt, fwd: Boolean)(implicit params: TLBundleParameters): Put = {
    new Put().Lit(_.source -> source.U, _.addr -> addr.U, _.mask -> mask.U, _.data -> data.U, _.fwd -> fwd.B)
  }
  // TODO: PutBurst is not randomizable
  // TODO: figure out if we can use a Vec[Put] and randomize it
  def PutBurst(source: Int, addr: BigInt, maskAndData: Seq[(Int, BigInt)], fwd: Boolean)(implicit params: TLBundleParameters): Seq[Put] = {
    maskAndData.map {
      case (mask, data) =>
        Put(source, addr, mask, data, fwd)
    }
  }
}

abstract class Atomic(implicit params: TLBundleParameters) extends TLTransactionA with HasData with HasFwd
// TODO: use a param mixin
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

class Intent(implicit params: TLBundleParameters) extends TLTransactionA with HasSize with HasFwd {
  val param: UInt = UInt(IntentParam.getWidth.W)
}
object IntentParam extends ChiselEnum {
  val PrefetchRead, PrefetchWrite = Value
}
object Intent {
  def apply(param: Int, size: Int, source: Int, addr: BigInt, mask: Int, data: BigInt, fwd: Boolean)(implicit params: TLBundleParameters): Intent = {
    new Intent().Lit(_.param -> IntentParam(param.U), _.size -> size.U, _.source -> source.U, _.addr -> addr.U, _.mask -> mask.U, _.fwd -> fwd.B)
  }
}

abstract class Acquire(implicit params: TLBundleParameters) extends TLTransactionA with HasSize {
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
abstract class TLTransactionB(implicit params: TLBundleParameters) extends TLTransaction with
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
abstract class TLTransactionC(implicit params: TLBundleParameters) extends TLTransaction with
  HasSource with HasSize with HasAddr {
  val param: UInt = UInt(TLCParam.getWidth.W)
}

class ProbeAck()(implicit params: TLBundleParameters) extends TLTransactionC
class ProbeAckData()(implicit params: TLBundleParameters) extends TLTransactionC with HasData
// TODO: put all permissions into one object
object TLCParam extends ChiselEnum {
  val TtoB, TtoN, BtoN, TtoT, BtoB, NtoN = Value
}
object ProbeAck {
  def apply(param: Int, size: Int, source: Int, addr: BigInt)(implicit params: TLBundleParameters): ProbeAck = {
    new ProbeAck().Lit(_.param -> TLCParam(param.U), _.size -> size.U, _.source -> source.U, _.addr -> addr.U)
  }
}
object ProbeAckData {
  def apply(param: Int, size: Int, source: Int, addr: BigInt, data: BigInt)(implicit params: TLBundleParameters): ProbeAckData = {
    new ProbeAckData().Lit(_.param -> TLCParam(param.U), _.size -> size.U, _.source -> source.U, _.addr -> addr.U, _.data -> data.U)
  }
}

class Release()(implicit params: TLBundleParameters) extends TLTransactionC
class ReleaseData()(implicit params: TLBundleParameters) extends TLTransactionC with HasData
object Release {
  def apply(param: Int, size: Int, source: Int, addr: BigInt)(implicit params: TLBundleParameters): Release = {
    new Release().Lit(_.param -> TLCParam(param.U), _.size -> size.U, _.source -> source.U, _.addr -> addr.U)
  }
}
object ReleaseData {
  def apply(param: Int, size: Int, source: Int, addr: BigInt, data: BigInt)(implicit params: TLBundleParameters): ReleaseData = {
    new ReleaseData().Lit(_.param -> TLCParam(param.U), _.size -> size.U, _.source -> source.U, _.addr -> addr.U, _.data -> data.U)
  }
}

// Channel D
// Note (TL-C): Transactions with fwd = True.B are sent via Channel C
abstract class TLTransactionD(implicit params: TLBundleParameters) extends TLTransaction with
  HasSource with HasSize

class AccessAck(implicit params: TLBundleParameters) extends TLTransactionD with HasDenied with HasFwd
class AccessAckData(implicit params: TLBundleParameters) extends TLTransactionD with HasDenied with HasFwd with HasData
object AccessAck {
  def apply(size: Int, source: Int, denied: Boolean, fwd: Boolean) (implicit params: TLBundleParameters): AccessAck = {
    new AccessAck().Lit(_.size -> size.U, _.source -> source.U, _.denied -> denied.B, _.fwd -> fwd.B)
  }
}
object AccessAckData {
  def apply(size: Int, source: Int, denied: Boolean, fwd: Boolean, data: BigInt) (implicit params: TLBundleParameters): AccessAckData = {
    new AccessAckData().Lit(_.size -> size.U, _.source -> source.U, _.denied -> denied.B, _.fwd -> fwd.B, _.data -> data.U)
  }
}

class HintAck(implicit params: TLBundleParameters) extends TLTransactionD with HasDenied with HasFwd
object HintAck {
  def apply(size: Int, source: Int, denied: Boolean, fwd: Boolean) (implicit params: TLBundleParameters): HintAck = {
    new HintAck().Lit(_.size -> size.U, _.source -> source.U, _.denied -> denied.B, _.fwd -> fwd.B)
  }
}

class Grant(implicit params: TLBundleParameters) extends TLTransactionD with HasDenied with HasSink {
  val param: UInt = UInt(GrantParam.getWidth.W)
}
class GrantData(implicit params: TLBundleParameters) extends TLTransactionD with HasDenied with HasSink with HasData {
  val param: UInt = UInt(GrantParam.getWidth.W)
}
object GrantParam extends ChiselEnum {
  val toT, toB, toN = Value
}
object Grant {
  def apply(param: Int, size: Int, source: Int, sink: Int, denied: Boolean) (implicit params: TLBundleParameters): Grant = {
    new Grant().Lit(_.param -> GrantParam(param.U), _.size -> size.U, _.source -> source.U, _.denied -> denied.B, _.sink-> sink.U)
  }
}
object GrantData {
  def apply(param: Int, size: Int, source: Int, sink: Int, denied: Boolean, data: BigInt) (implicit params: TLBundleParameters): GrantData = {
    new GrantData().Lit(_.param -> GrantParam(param.U), _.size -> size.U, _.source -> source.U, _.denied -> denied.B, _.sink-> sink.U, _.data -> data.U)
  }
}

class ReleaseAck(implicit params: TLBundleParameters) extends TLTransactionD
object ReleaseAck {
  def apply(size: Int, source: Int)(implicit params: TLBundleParameters): ReleaseAck = {
    new ReleaseAck().Lit(_.size -> size.U, _.source -> source.U)
  }
}

// Channel E
abstract class TLTransactionE(implicit params: TLBundleParameters) extends TLTransaction with HasSink
class GrantAck(implicit params: TLBundleParameters) extends TLTransactionE
object GrantAck {
  def apply(sink: Int)(implicit params: TLBundleParameters): GrantAck = {
    new GrantAck().Lit(_.sink -> sink.U)
  }
}
