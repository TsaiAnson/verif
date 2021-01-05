package verif

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.ChiselEnum
import freechips.rocketchip.tilelink.{TLBundleA, TLBundleB, TLBundleBase, TLBundleC, TLBundleD, TLBundleE, TLBundleParameters, TLMessages}

import scala.collection.mutable

case class TLBundleATx(p: TLBundleParameters) extends Bundle with Transaction {
  val a = new TLBundleA(p) with Transaction
}

// TLTransactions are just TLChannel Bundle literals
// There are some helper methods here to construct these literals for user stimulus
package object TLTransaction {
  def Get(addr: BigInt, size: Int, mask: Int, source: Int)(implicit params: TLBundleParameters): TLBundleA = {
    // TODO: add checks for truncation
    new TLBundleA(params).Lit(
      _.opcode -> TLMessages.Get,
      _.param -> 0.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> addr.U,
      _.mask -> mask.U,
      _.corrupt -> 0.U,
      _.data -> 0.U
    )
  }
}
/*
//case class TLTransaction(implicit val params: TLBundleParameters) extends TLBundleA(params) with Transaction

abstract case class TLTransaction[T <: TLBundleBase](b: (TLBundleParameters) => T)(implicit val params: TLBundleParameters) extends Bundle with Transaction {
  val litFns = mutable.MutableList[T => (Data, Data)]()
  final def toBundle: T = {
    b(params).Lit(litFns :_*)
  }
}

// TODO: this is untyped; in Dotty we can have proper adhoc union types
//  but using typeclasses to restrict T to TLBundleA,B,C,D is too clunky
trait HasSource[T <: TLBundleBase] { this: TLTransaction[T] =>
  val source: UInt = UInt(this.params.sourceBits.W)
  def sourceFn: T => (Data, Data) = {
    t: T => t match {
      case t: TLBundleA => t.source -> source
      case t: TLBundleB => t.source -> source
      case t: TLBundleC => t.source -> source
      case t: TLBundleD => t.source -> source
      case _ => ???
    }
  }
  litFns += sourceFn
}

trait HasAddr[T <: TLBundleBase] { this: TLTransaction[T] =>
  val addr: UInt = UInt(this.params.addressBits.W)
  def addrFn: T => (Data, Data) = {
    t: T => t match {
      case t: TLBundleA => t.address -> addr
      case t: TLBundleB => t.address -> addr
      case t: TLBundleC => t.address -> addr
      case _ => ???
    }
  }
  litFns += addrFn
}

trait HasMask[T <: TLBundleBase] { this: TLTransaction[T] =>
  val mask: UInt = UInt((this.params.dataBits/8).W)
  def maskFn: T => (Data, Data) = {
    t: T => t match {
      case t: TLBundleA => t.mask -> mask
      case t: TLBundleB => t.mask -> mask
      case _ => ???
    }
  }
  litFns += maskFn
}

trait HasSize[T <: TLBundleBase] { this: TLTransaction[T] =>
  val size: UInt = UInt(this.params.sizeBits.W)
  def sizeFn: T => (Data, Data) = {
    t: T => t match {
      case t: TLBundleA => t.size -> size
      case t: TLBundleB => t.size -> size
      case t: TLBundleC => t.size -> size
      case t: TLBundleD => t.size -> size
      case _ => ???
    }
  }
  litFns += sizeFn
}

trait HasData[T <: TLBundleBase] { this: TLTransaction[T] =>
  val data: UInt = UInt(this.params.dataBits.W)
  def dataFn: T => (Data, Data) = {
    t: T => t match {
      case t: TLBundleA => t.data -> data
      case t: TLBundleC => t.data -> data
      case t: TLBundleD => t.data -> data
      case _ => ???
    }
  }
  litFns += dataFn
}

trait HasDenied[T <: TLBundleBase] { this: TLTransaction[T] =>
  val denied: Bool = Bool()
  def deniedFn: T => (Data, Data) = {
    t: T => t match {
      case t: TLBundleD => t.denied -> denied
      case _ => ???
    }
  }
  litFns += deniedFn
}

trait HasSink[T <: TLBundleBase] { this: TLTransaction[T] =>
  val sink: UInt = UInt(this.params.sinkBits.W)
  def sinkFn: T => (Data, Data) = {
    t: T => t match {
      case t: TLBundleD => t.sink -> sink
      case t: TLBundleE => t.sink -> sink
      case _ => ???
    }
  }
  litFns += sinkFn
}

// Note (TL-C): A Transactions with fwd = True.B are sent via Channel B
// Note (TL-C): Transactions with fwd = True.B are sent via Channel C
trait HasFwd { this: TLTransaction =>
  val fwd: Bool = Bool()
}

// Channel A
// TODO: get rid of implicits by constructing a base class
// TODO: add checks for truncation when converting Scala types to Chisel hardware types
// TODO: reorganize parameter order and add sensible defaults
abstract class TLTransactionA(implicit params: TLBundleParameters) extends TLTransaction[TLBundleA](params => new TLBundleA(params)) with
  HasSource[TLBundleA] with HasMask[TLBundleA] with HasAddr[TLBundleA] {
  litFns ++= Seq(
    (t: TLBundleA) => t.corrupt -> false.B
  )
}

class Get(implicit params: TLBundleParameters) extends TLTransactionA with HasSize[TLBundleA] {
  litFns ++= Seq(
    (t: TLBundleA) => t.param -> 0.U,
    (t: TLBundleA) => t.opcode -> TLMessages.Get,
    (t: TLBundleA) => t.data -> 0.U
  )
}

// TODO: typed size and mask fields
object Get {
  def apply(addr: BigInt, size: Int, mask: Int, source: Int)(implicit params: TLBundleParameters): Get = {
    new Get().Lit(_.size -> size.U, _.source -> source.U, _.addr -> addr.U, _.mask -> mask.U)
  }
  def apply(addr: UInt, size: UInt, mask: UInt, source: UInt)(implicit params: TLBundleParameters): Get = {
    new Get().Lit(_.size -> size, _.source -> source, _.addr -> addr, _.mask -> mask)
  }
}

class Put(implicit params: TLBundleParameters) extends TLTransactionA with HasData[TLBundleA]
object Put{
  // TODO: create a another constructor without mask (implicitly PutFull)
  def apply(source: Int, addr: BigInt, mask: Int, data: BigInt)(implicit params: TLBundleParameters): Put = {
    new Put().Lit(_.source -> source.U, _.addr -> addr.U, _.mask -> mask.U, _.data -> data.U)
  }
  // TODO: PutBurst is not randomizable
  // TODO: figure out if we can use a Vec[Put] and randomize it
  def PutBurst(source: Int, addr: BigInt, maskAndData: Seq[(Int, BigInt)], fwd: Boolean)(implicit params: TLBundleParameters): Seq[Put] = {
    maskAndData.map {
      case (mask, data) =>
        Put(source, addr, mask, data)
    }
  }
}

abstract class Atomic(implicit params: TLBundleParameters) extends TLTransactionA with HasData[TLBundleA]
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
    new Arithmetic().Lit(_.param -> ArithmeticParam(param.U), _.source -> source.U, _.addr -> addr.U, _.mask -> mask.U, _.data -> data.U)
  }
}
object Logical {
  def apply(param: Int, source: Int, addr: BigInt, mask: Int, data: BigInt, fwd: Boolean)(implicit params: TLBundleParameters): Logical = {
    new Logical().Lit(_.param -> LogicalParam(param.U), _.source -> source.U, _.addr -> addr.U, _.mask -> mask.U, _.data -> data.U)
  }
}

class Intent(implicit params: TLBundleParameters) extends TLTransactionA with HasSize[TLBundleA] {
  val param: UInt = UInt(IntentParam.getWidth.W)
}
object IntentParam extends ChiselEnum {
  val PrefetchRead, PrefetchWrite = Value
}
object Intent {
  def apply(param: Int, size: Int, source: Int, addr: BigInt, mask: Int, fwd: Boolean)(implicit params: TLBundleParameters): Intent = {
    new Intent().Lit(_.param -> IntentParam(param.U), _.size -> size.U, _.source -> source.U, _.addr -> addr.U, _.mask -> mask.U)
  }
}

abstract class Acquire(implicit params: TLBundleParameters) extends TLTransactionA with HasSize[TLBundleA] {
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
abstract class TLTransactionB(implicit params: TLBundleParameters) extends TLTransaction[TLBundleB](params => new TLBundleB(params)) with
  HasSource[TLBundleB] with HasMask[TLBundleB] with HasAddr[TLBundleB] with HasSize[TLBundleB] {
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
abstract class TLTransactionC(implicit params: TLBundleParameters) extends TLTransaction[TLBundleC](p => new TLBundleC(p)) with
  HasSource[TLBundleC] with HasSize[TLBundleC] with HasAddr[TLBundleC] {
  val param: UInt = UInt(TLCParam.getWidth.W)
}

class ProbeAck()(implicit params: TLBundleParameters) extends TLTransactionC
class ProbeAckData()(implicit params: TLBundleParameters) extends TLTransactionC with HasData[TLBundleC]
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
class ReleaseData()(implicit params: TLBundleParameters) extends TLTransactionC with HasData[TLBundleC]
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
abstract class TLTransactionD(implicit params: TLBundleParameters) extends TLTransaction[TLBundleD](p => new TLBundleD(p)) with
  HasSource[TLBundleD] with HasSize[TLBundleD]

class AccessAck(implicit params: TLBundleParameters) extends TLTransactionD with HasDenied[TLBundleD]
class AccessAckData(implicit params: TLBundleParameters) extends TLTransactionD with HasDenied[TLBundleD] with HasData[TLBundleD]
object AccessAck {
  def apply(size: Int, source: Int, denied: Boolean, fwd: Boolean) (implicit params: TLBundleParameters): AccessAck = {
    new AccessAck().Lit(_.size -> size.U, _.source -> source.U, _.denied -> denied.B)
  }
}
object AccessAckData {
  def apply(size: Int, source: Int, denied: Boolean, fwd: Boolean, data: BigInt) (implicit params: TLBundleParameters): AccessAckData = {
    new AccessAckData().Lit(_.size -> size.U, _.source -> source.U, _.denied -> denied.B, _.data -> data.U)
  }
}

class HintAck(implicit params: TLBundleParameters) extends TLTransactionD with HasDenied[TLBundleD]
object HintAck {
  def apply(size: Int, source: Int, denied: Boolean, fwd: Boolean) (implicit params: TLBundleParameters): HintAck = {
    new HintAck().Lit(_.size -> size.U, _.source -> source.U, _.denied -> denied.B)
  }
}

class Grant(implicit params: TLBundleParameters) extends TLTransactionD with HasDenied[TLBundleD] with HasSink[TLBundleD] {
  val param: UInt = UInt(GrantParam.getWidth.W)
}
class GrantData(implicit params: TLBundleParameters) extends TLTransactionD with HasDenied[TLBundleD] with HasSink[TLBundleD] with HasData[TLBundleD] {
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
abstract class TLTransactionE(implicit params: TLBundleParameters) extends TLTransaction[TLBundleE](p => new TLBundleE(p)) with HasSink[TLBundleE]
class GrantAck(implicit params: TLBundleParameters) extends TLTransactionE
object GrantAck {
  def apply(sink: Int)(implicit params: TLBundleParameters): GrantAck = {
    new GrantAck().Lit(_.sink -> sink.U)
  }
}
*/