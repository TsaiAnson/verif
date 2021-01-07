package verif

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util.log2Ceil
import freechips.rocketchip.tilelink.{TLBundleA, TLBundleB, TLBundleBase, TLBundleC, TLBundleD, TLBundleE, TLBundleParameters, TLChannel, TLMessages}

// TLTransactions are just TLChannel Bundle literals
// There are some helper methods here to construct these literals for user stimulus
package object TLTransaction {
  // TL-UH
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

  // TL-UL
  def Get(addr: BigInt)(implicit params: TLBundleParameters): TLBundleA = {
    Get(addr = addr, size = log2Ceil(params.dataBits/8), mask = 2^(params.dataBits/8) - 1, source = 0)
  }

  def Put(addr: BigInt, data: BigInt, mask: Int, size: Int, source: Int, partialHint: Boolean = false)(implicit params: TLBundleParameters): TLBundleA = {
    val opcode = if (partialHint || (mask != (2^(params.dataBits/8) - 1))) TLMessages.PutPartialData else TLMessages.PutFullData
    // TODO: add checks for truncation
    new TLBundleA(params).Lit(
      _.opcode -> opcode,
      _.param -> 0.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> addr.U,
      _.mask -> mask.U,
      _.corrupt -> 0.U,
      _.data -> data.U
    )
  }

  // TL-UL
  def Put(addr: BigInt, data: BigInt, mask: Int)(implicit params: TLBundleParameters): TLBundleA = {
    Put(addr = addr, data = data, mask = mask, size = log2Ceil(params.dataBits/8), source = 0)
  }

  def Put(addr: BigInt, data: BigInt)(implicit params: TLBundleParameters): TLBundleA = {
    Put(addr = addr, data = data, mask = 2^(params.dataBits/8) - 1, size = log2Ceil(params.dataBits/8), source = 0)
  }

  // No masks assume PutFull
  def PutBurst(addr: BigInt, data: Seq[BigInt], source: Int = 0)(implicit params: TLBundleParameters): Seq[TLBundleA] = {
    data.map {
      (d: BigInt) => Put(addr = addr, data = d, mask = 2^(params.dataBits/8) - 1, size = log2Ceil(params.dataBits/8 * data.size), source = source)
    }
  }

  // Masks assume PutPartial
  def PutBurst(addr: BigInt, data: Seq[BigInt], mask: Seq[Int], source: Int = 0)(implicit params: TLBundleParameters): Seq[TLBundleA] = {
    (mask zip data).map {
      (md : (Int, BigInt)) => Put(addr = addr, data = md._2, mask = md._1, size = log2Ceil(params.dataBits/8 * data.size), source = source, partialHint = true)
    }
  }

  def Arith(param: Int, addr: BigInt, data: BigInt, mask: Int, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleA = {
    // TODO: add checks for truncation
    new TLBundleA(params).Lit(
      _.opcode -> TLMessages.ArithmeticData,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> addr.U,
      _.mask -> mask.U,
      _.corrupt -> 0.U,
      _.data -> data.U
    )
  }

  def Arith(param: Int, addr: BigInt, data: BigInt, source: Int = 0)(implicit params: TLBundleParameters): TLBundleA = {
    Arith(param = param, addr = addr, data = data, mask = 2^(params.dataBits/8) - 1, size = log2Ceil(params.dataBits/8), source = source)
  }

  def ArithBurst(param: Int, addr: BigInt, data: Seq[BigInt], source: Int = 0)(implicit params: TLBundleParameters): Seq[TLBundleA] = {
    data.map {
      (d: BigInt) => Arith(param = param, addr = addr, data = d, mask = 2^(params.dataBits/8) - 1, size = log2Ceil(params.dataBits/8), source = source)
    }
  }

  def Logic(param: Int, addr: BigInt, data: BigInt, mask: Int, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleA = {
    // TODO: add checks for truncation
    new TLBundleA(params).Lit(
      _.opcode -> TLMessages.LogicalData,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> addr.U,
      _.mask -> mask.U,
      _.corrupt -> 0.U,
      _.data -> data.U
    )
  }

  def Logic(param: Int, addr: BigInt, data: BigInt, source: Int = 0)(implicit params: TLBundleParameters): TLBundleA = {
    Logic(param = param, addr = addr, data = data, mask = 2^(params.dataBits/8) - 1, size = log2Ceil(params.dataBits/8), source = source)
  }

  def LogicBurst(param: Int, addr: BigInt, data: Seq[BigInt], source: Int = 0)(implicit params: TLBundleParameters): Seq[TLBundleA] = {
    data.map {
      (d: BigInt) => Arith(param = param, addr = addr, data = d, mask = 2^(params.dataBits/8) - 1, size = log2Ceil(params.dataBits/8), source = source)
    }
  }

  def Intent(param: Int, addr: BigInt, mask: Int, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleA = {
    // TODO: add checks for truncation
    new TLBundleA(params).Lit(
      _.opcode -> TLMessages.Hint,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> addr.U,
      _.mask -> mask.U,
      _.corrupt -> 0.U,
      _.data -> 0.U
    )
  }

  def Intent(param: Int, addr: BigInt, source: Int = 0)(implicit params: TLBundleParameters): TLBundleA = {
    Intent(param = param, addr = addr, mask = 2^(params.dataBits/8) - 1, size = log2Ceil(params.dataBits/8), source = source)
  }

  def Intent(param: Int, addr: BigInt, size: Int, source: Int = 0)(implicit params: TLBundleParameters): TLBundleA = {
    Intent(param = param, addr = addr, mask = 2^(params.dataBits/8) - 1, size = size, source = source)
  }

  def AcquireBlock(param: Int, addr: BigInt, mask: Int, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleA = {
    // TODO: add checks for truncation
    new TLBundleA(params).Lit(
      _.opcode -> TLMessages.AcquireBlock,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> addr.U,
      _.mask -> mask.U,
      _.corrupt -> 0.U,
      _.data -> 0.U
    )
  }

  def AcquireBlock(param: Int, addr: BigInt, size: Int, source: Int = 0)(implicit params: TLBundleParameters): TLBundleA = {
    AcquireBlock(param = param, addr = addr, mask = 2^(params.dataBits/8) - 1, size = size, source = source)
  }

  def AcquirePerm(param: Int, addr: BigInt, mask: Int, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleA = {
    // TODO: add checks for truncation
    new TLBundleA(params).Lit(
      _.opcode -> TLMessages.AcquirePerm,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> addr.U,
      _.mask -> mask.U,
      _.corrupt -> 0.U,
      _.data -> 0.U
    )
  }

  def AcquirePerm(param: Int, addr: BigInt, size: Int, source: Int = 0)(implicit params: TLBundleParameters): TLBundleA = {
    AcquirePerm(param = param, addr = addr, mask = 2^(params.dataBits/8) - 1, size = size, source = source)
  }

  def ProbeBlock(param: Int, addr: BigInt, mask: Int, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleB = {
    // TODO: add checks for truncation
    new TLBundleB(params).Lit(
      _.opcode -> 6.U, // TLMessages does not have ProbeBlock
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> addr.U,
      _.mask -> mask.U,
      _.corrupt -> 0.U,
      _.data -> 0.U
    )
  }

  def ProbeBlock(param: Int, addr: BigInt, size: Int, source: Int = 0)(implicit params: TLBundleParameters): TLBundleB = {
    ProbeBlock(param = param, addr = addr, mask = 2^(params.dataBits/8) - 1, size = size, source = source)
  }

  def ProbePerm(param: Int, addr: BigInt, mask: Int, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleB = {
    // TODO: add checks for truncation
    new TLBundleB(params).Lit(
      _.opcode -> 7.U, // TLMessages does not have ProbePerm
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> addr.U,
      _.mask -> mask.U,
      _.corrupt -> 0.U,
      _.data -> 0.U
    )
  }

  def ProbePerm(param: Int, addr: BigInt, size: Int, source: Int = 0)(implicit params: TLBundleParameters): TLBundleB = {
    ProbeBlock(param = param, addr = addr, mask = 2^(params.dataBits/8) - 1, size = size, source = source)
  }

  def ProbeAck(param: Int, addr: BigInt, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleC = {
    // TODO: add checks for truncation
    new TLBundleC(params).Lit(
      _.opcode -> TLMessages.ProbeAckData,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> addr.U,
      _.corrupt -> 0.U,
      _.data -> 0.U
    )
  }

  def ProbeAckData(param: Int, addr: BigInt, data: BigInt, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleC = {
    // TODO: add checks for truncation
    new TLBundleC(params).Lit(
      _.opcode -> TLMessages.ProbeAckData,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> addr.U,
      _.corrupt -> 0.U,
      _.data -> 0.U
    )
  }

  def ProbeAckDataBurst(param: Int, addr: BigInt, data: Seq[BigInt], source: Int)(implicit params: TLBundleParameters): Seq[TLBundleC] = {
    data.map {
      (d: BigInt) => ProbeAckData(param = param, addr = addr, data = d, size = log2Ceil(params.dataBits/8), source = source)
    }
  }

  def Release(param: Int, addr: BigInt, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleC = {
    // TODO: add checks for truncation
    new TLBundleC(params).Lit(
      _.opcode -> TLMessages.Release,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> addr.U,
      _.corrupt -> 0.U,
      _.data -> 0.U
    )
  }

  def ReleaseData(param: Int, addr: BigInt, data: BigInt, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleC = {
    // TODO: add checks for truncation
    new TLBundleC(params).Lit(
      _.opcode -> TLMessages.Release,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> addr.U,
      _.corrupt -> 0.U,
      _.data -> data.U
    )
  }

  def ReleaseDataBurst(param: Int, addr: BigInt, data: Seq[BigInt], source: Int)(implicit params: TLBundleParameters): Seq[TLBundleC] = {
    data.map {
      (d: BigInt) => ReleaseData(param = param, addr = addr, data = d, size = log2Ceil(params.dataBits/8), source = source)
    }
  }

  def AccessAck(denied: Int, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleD = {
    // TODO: add checks for truncation
    new TLBundleD(params).Lit(
      _.opcode -> TLMessages.AccessAck,
      _.param -> 0.U,
      _.size -> size.U,
      _.source -> source.U,
      _.sink -> 0.U,
      _.denied -> denied.U,
      _.corrupt -> 0.U,
      _.data -> 0.U
    )
  }

  def AccessAck(denied: Int, source: Int = 0)(implicit params: TLBundleParameters): TLBundleD = {
    AccessAck(denied = denied, size = log2Ceil(params.dataBits/8), source = source)
  }

  def AccessAckData(data: BigInt, denied: Int, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleD = {
    // TODO: add checks for truncation
    new TLBundleD(params).Lit(
      _.opcode -> TLMessages.AccessAckData,
      _.param -> 0.U,
      _.size -> size.U,
      _.source -> source.U,
      _.sink -> 0.U,
      _.denied -> denied.U,
      _.corrupt -> 0.U,
      _.data -> data.U
    )
  }

  def AccessAckData(data: BigInt, denied: Int, source: Int = 0)(implicit params: TLBundleParameters): TLBundleD = {
    AccessAckData(data = data, denied = denied, size = log2Ceil(params.dataBits/8), source = source)
  }

  def AccessAckDataBurst(data: Seq[BigInt], denied: Int, source: Int = 0)(implicit params: TLBundleParameters): Seq[TLBundleD] = {
    data.map {
      (d: BigInt) => AccessAckData(data = d, denied = denied, size = log2Ceil(params.dataBits/8 * data.size), source = source)
    }
  }

  def HintAck(denied: Int, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleD = {
    // TODO: add checks for truncation
    new TLBundleD(params).Lit(
      _.opcode -> TLMessages.HintAck,
      _.param -> 0.U,
      _.size -> size.U,
      _.source -> source.U,
      _.sink -> 0.U,
      _.denied -> denied.U,
      _.corrupt -> 0.U,
      _.data -> 0.U
    )
  }

  def HintAck(denied: Int, source: Int = 0)(implicit params: TLBundleParameters): TLBundleD = {
    HintAck(denied = denied, size = log2Ceil(params.dataBits/8), source = source)
  }

  def Grant(param: Int, denied: Int, size: Int, source: Int, sink: Int)(implicit params: TLBundleParameters): TLBundleD = {
    // TODO: add checks for truncation
    new TLBundleD(params).Lit(
      _.opcode -> TLMessages.Grant,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.sink -> sink.U,
      _.denied -> denied.U,
      _.corrupt -> 0.U,
      _.data -> 0.U
    )
  }

  def GrantData(param: Int, data: BigInt, denied: Int, size: Int, source: Int, sink: Int)(implicit params: TLBundleParameters): TLBundleD = {
    // TODO: add checks for truncation
    new TLBundleD(params).Lit(
      _.opcode -> TLMessages.GrantData,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.sink -> sink.U,
      _.denied -> denied.U,
      _.corrupt -> 0.U,
      _.data -> data.U
    )
  }

  def GrantData(param: Int, data: BigInt, denied: Int, sink: Int, source: Int = 0)(implicit params: TLBundleParameters): TLBundleD = {
    GrantData(param = param, data = data, denied = denied, size = log2Ceil(params.dataBits/8), source = source, sink = sink)
  }

  def GrantDataBurst(param: Int, data: Seq[BigInt], denied: Int, sink: Int, source: Int = 0)(implicit params: TLBundleParameters): Seq[TLBundleD] = {
    data.map {
      (d: BigInt) => GrantData(param = param, data = d, denied = denied, size = log2Ceil(params.dataBits/8 * data.size), source = source, sink = sink)
    }
  }

  def ReleaseAck(size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleD = {
    // TODO: add checks for truncation
    new TLBundleD(params).Lit(
      _.opcode -> TLMessages.ReleaseAck,
      _.param -> 0.U,
      _.size -> size.U,
      _.source -> source.U,
      _.sink -> 0.U,
      _.denied -> 0.U,
      _.corrupt -> 0.U,
      _.data -> 0.U
    )
  }

  def GrantAck(sink: Int)(implicit params: TLBundleParameters): TLBundleE = {
    new TLBundleE(params).Lit(
      _.sink -> sink.U
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