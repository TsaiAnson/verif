package verif

import scala.math.pow
import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util.log2Ceil
import freechips.rocketchip.tilelink.{TLBundle, TLBundleA, TLBundleB, TLBundleC, TLBundleD, TLBundleE, TLBundleParameters, TLChannel}

// TLTransactions are just TLChannel Bundle literals
// There are some helper methods here to construct these literals for user stimulus
package object TLTransaction {
  type TLTransaction = TLChannel

  // copied from rocket-chip
  // converted from raw listing of perm type to UInt to sealed types
  object TLPermissions {
    sealed trait Permission {
      def value: UInt
    }

    // Cap types (Grant = new permissions, Probe = permisions <= target)
    sealed trait Cap extends Permission
    case class toT() extends Cap {
      override def value: UInt = 0.U
    }
    case class toB() extends Cap {
      override def value: UInt = 1.U
    }
    case class toN() extends Cap {
      override def value: UInt = 2.U
    }

    // Grow types (Acquire = permissions >= target)
    sealed trait Grow extends Permission
    case class NtoB() extends Grow {
      override def value: UInt = 0.U
    }
    case class NtoT() extends Grow {
      override def value: UInt = 1.U
    }
    case class BtoT() extends Grow {
      override def value: UInt = 2.U
    }

    // Prune types (ProbeAck, Release)
    sealed trait Prune extends Permission
    case class TtoB() extends Prune {
      override def value: UInt = 0.U
    }
    case class TtoN() extends Prune {
      override def value: UInt = 1.U
    }
    case class BtoN() extends Prune {
      override def value: UInt = 2.U
    }

    // Report types (ProbeAck, Release)
    sealed trait Report extends Permission
    case class ToT() extends Report {
      override def value: UInt = 3.U
    }
    case class BtoB() extends Report {
      override def value: UInt = 4.U
    }
    case class NtoN() extends Report {
      override def value: UInt = 5.U
    }
  }

  // Copied from rocket-chip
  // rocket-chip version uses Chisel 2 compat layer which doesn't support UInt literals
  object TLOpcodes {
    //                            A    B    C    D    E
    val PutFullData    = 0 //     .    .                   => AccessAck
    val PutPartialData = 1 //     .    .                   => AccessAck
    val ArithmeticData = 2 //     .    .                   => AccessAckData
    val LogicalData    = 3 //     .    .                   => AccessAckData
    val Get            = 4 //     .    .                   => AccessAckData
    val Hint           = 5 //     .    .                   => HintAck (note: Hint = Intent)
    val AcquireBlock   = 6 //     .                        => Grant[Data]
    val AcquirePerm    = 7 //     .                        => Grant[Data]
    val ProbeBlock     = 6 //          .                   => ProbeAck[Data]
    val ProbePerm      = 7 //          .                   => ProbeAck
    val AccessAck      = 0 //               .    .
    val AccessAckData  = 1 //               .    .
    val HintAck        = 2 //               .    .
    val ProbeAck       = 4 //               .
    val ProbeAckData   = 5 //               .
    val Release        = 6 //               .              => ReleaseAck
    val ReleaseData    = 7 //               .              => ReleaseAck
    val Grant          = 4 //                    .         => GrantAck
    val GrantData      = 5 //                    .         => GrantAck
    val ReleaseAck     = 6 //                    .
    val GrantAck       = 0 //                         .
  }

  // **************************************************************
  // ************************ CHANNEL A ***************************
  // **************************************************************

  def fullMask(implicit p: TLBundleParameters): Int = {
    assert(p.dataBits % 8 == 0)
    pow(2, p.dataBits/8).toInt - 1
  }

  // TL-UH
  def Get(addr: BigInt, size: Int, mask: Int, source: Int)(implicit params: TLBundleParameters): TLBundleA = {
    // TODO: add checks for truncation
    new TLBundleA(params).Lit(
      _.opcode -> TLOpcodes.Get.U,
      _.param -> 0.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> addr.U,
      _.mask -> mask.U,
      _.corrupt -> 0.B,
      _.data -> 0.U
    )
  }

  // TL-UL
  def Get(addr: BigInt)(implicit params: TLBundleParameters): TLBundleA = {
    Get(addr = addr, size = log2Ceil(params.dataBits/8), mask = fullMask, source = 0)
  }

  def Put(addr: BigInt, data: BigInt, mask: Int, size: Int, source: Int, partialHint: Boolean = false)(implicit params: TLBundleParameters): TLBundleA = {
    val opcode = if (partialHint || (mask != fullMask)) TLOpcodes.PutPartialData else TLOpcodes.PutFullData
    // TODO: add checks for truncation
    new TLBundleA(params).Lit(
      _.opcode -> opcode.U,
      _.param -> 0.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> addr.U,
      _.mask -> mask.U,
      _.corrupt -> 0.B,
      _.data -> data.U
    )
  }

  // TL-UL
  def Put(addr: BigInt, data: BigInt, mask: Int)(implicit params: TLBundleParameters): TLBundleA = {
    Put(addr = addr, data = data, mask = mask, size = log2Ceil(params.dataBits/8), source = 0)
  }

  def Put(addr: BigInt, data: BigInt)(implicit params: TLBundleParameters): TLBundleA = {
    Put(addr = addr, data = data, mask = fullMask, size = log2Ceil(params.dataBits/8), source = 0)
  }

  // No masks assume PutFull
  def PutBurst(addr: BigInt, data: Seq[BigInt], source: Int)(implicit params: TLBundleParameters): Seq[TLBundleA] = {
    data.map {
      (d: BigInt) => Put(addr = addr, data = d, mask = fullMask, size = log2Ceil(params.dataBits/8 * data.size), source = source)
    }
  }

  // Masks assume PutPartial
  def PutBurst(addr: BigInt, data: Seq[BigInt], mask: Seq[Int], source: Int)(implicit params: TLBundleParameters): Seq[TLBundleA] = {
    (mask zip data).map {
      (md : (Int, BigInt)) => Put(addr = addr, data = md._2, mask = md._1, size = log2Ceil(params.dataBits/8 * data.size), source = source, partialHint = true)
    }
  }

  def Arith(param: Int, addr: BigInt, data: BigInt, mask: Int, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleA = {
    // TODO: add checks for truncation
    new TLBundleA(params).Lit(
      _.opcode -> TLOpcodes.ArithmeticData.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> addr.U,
      _.mask -> mask.U,
      _.corrupt -> 0.B,
      _.data -> data.U
    )
  }

  def Arith(param: Int, addr: BigInt, data: BigInt, source: Int = 0)(implicit params: TLBundleParameters): TLBundleA = {
    Arith(param = param, addr = addr, data = data, mask = fullMask, size = log2Ceil(params.dataBits/8), source = source)
  }

  def ArithBurst(param: Int, addr: BigInt, data: Seq[BigInt], source: Int = 0)(implicit params: TLBundleParameters): Seq[TLBundleA] = {
    data.map {
      (d: BigInt) => Arith(param, addr, d, source)
    }
  }

  def Logic(param: Int, addr: BigInt, data: BigInt, mask: Int, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleA = {
    // TODO: add checks for truncation
    new TLBundleA(params).Lit(
      _.opcode -> TLOpcodes.LogicalData.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> addr.U,
      _.mask -> mask.U,
      _.corrupt -> 0.B,
      _.data -> data.U
    )
  }

  def Logic(param: Int, addr: BigInt, data: BigInt, source: Int = 0)(implicit params: TLBundleParameters): TLBundleA = {
    Logic(param = param, addr = addr, data = data, mask = fullMask, size = log2Ceil(params.dataBits/8), source = source)
  }

  def LogicBurst(param: Int, addr: BigInt, data: Seq[BigInt], source: Int = 0)(implicit params: TLBundleParameters): Seq[TLBundleA] = {
    data.map {
      (d: BigInt) => Logic(param, addr, d, source)
    }
  }

  def Intent(param: Int, addr: BigInt, mask: Int, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleA = {
    // TODO: add checks for truncation
    new TLBundleA(params).Lit(
      _.opcode -> TLOpcodes.Hint.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> addr.U,
      _.mask -> mask.U,
      _.corrupt -> 0.B,
      _.data -> 0.U
    )
  }

  def Intent(param: Int, addr: BigInt, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleA = {
    Intent(param = param, addr = addr, mask = fullMask, size = size, source = source)
  }

  def Intent(param: Int, addr: BigInt, source: Int = 0)(implicit params: TLBundleParameters): TLBundleA = {
    Intent(param = param, addr = addr, mask = fullMask, size = log2Ceil(params.dataBits/8), source = source)
  }

  def AcquireBlock(param: Int, addr: BigInt, mask: Int, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleA = {
    // TODO: add checks for truncation
    new TLBundleA(params).Lit(
      _.opcode -> TLOpcodes.AcquireBlock.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> addr.U,
      _.mask -> mask.U,
      _.corrupt -> 0.B,
      _.data -> 0.U
    )
  }

  def AcquireBlock(param: Int, addr: BigInt, size: Int, source: Int = 0)(implicit params: TLBundleParameters): TLBundleA = {
    AcquireBlock(param = param, addr = addr, mask = fullMask, size = size, source = source)
  }

  def AcquirePerm(param: Int, addr: BigInt, mask: Int, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleA = {
    // TODO: add checks for truncation
    new TLBundleA(params).Lit(
      _.opcode -> TLOpcodes.AcquirePerm.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> addr.U,
      _.mask -> mask.U,
      _.corrupt -> 0.B,
      _.data -> 0.U
    )
  }

  def AcquirePerm(param: Int, addr: BigInt, size: Int, source: Int = 0)(implicit params: TLBundleParameters): TLBundleA = {
    AcquirePerm(param = param, addr = addr, mask = fullMask, size = size, source = source)
  }


  // **************************************************************
  // ************************ CHANNEL B ***************************
  // **************************************************************

  def ProbeBlock(param: Int, addr: BigInt, mask: Int, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleB = {
    // TODO: add checks for truncation
    new TLBundleB(params).Lit(
      _.opcode -> TLOpcodes.ProbeBlock.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> addr.U,
      _.mask -> mask.U,
      _.corrupt -> 0.B,
      _.data -> 0.U
    )
  }

  def ProbeBlock(param: Int, addr: BigInt, size: Int, source: Int = 0)(implicit params: TLBundleParameters): TLBundleB = {
    ProbeBlock(param, addr, fullMask, size, source)
  }

  def ProbePerm(param: Int, addr: BigInt, mask: Int, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleB = {
    // TODO: add checks for truncation
    new TLBundleB(params).Lit(
      _.opcode -> TLOpcodes.ProbePerm.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> addr.U,
      _.mask -> mask.U,
      _.corrupt -> 0.B,
      _.data -> 0.U
    )
  }

  def ProbePerm(param: Int, addr: BigInt, size: Int, source: Int = 0)(implicit params: TLBundleParameters): TLBundleB = {
    ProbePerm(param, addr, fullMask, size, source)
  }

  // **************************************************************
  // ************************ CHANNEL C ***************************
  // **************************************************************

  def ProbeAck(param: Int, addr: BigInt, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleC = {
    // TODO: add checks for truncation
    new TLBundleC(params).Lit(
      _.opcode -> TLOpcodes.ProbeAckData.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> addr.U,
      _.corrupt -> 0.B,
      _.data -> 0.U
    )
  }

  def ProbeAckData(param: Int, addr: BigInt, data: BigInt, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleC = {
    // TODO: add checks for truncation
    new TLBundleC(params).Lit(
      _.opcode -> TLOpcodes.ProbeAckData.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> addr.U,
      _.corrupt -> 0.B,
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
      _.opcode -> TLOpcodes.Release.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> addr.U,
      _.corrupt -> 0.B,
      _.data -> 0.U
    )
  }

  def ReleaseData(param: Int, addr: BigInt, data: BigInt, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleC = {
    // TODO: add checks for truncation
    new TLBundleC(params).Lit(
      _.opcode -> TLOpcodes.ReleaseData.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> addr.U,
      _.corrupt -> 0.B,
      _.data -> data.U
    )
  }

  def ReleaseDataBurst(param: Int, addr: BigInt, data: Seq[BigInt], source: Int)(implicit params: TLBundleParameters): Seq[TLBundleC] = {
    data.map {
      (d: BigInt) => ReleaseData(param = param, addr = addr, data = d, size = log2Ceil(params.dataBits/8), source = source)
    }
  }

  // **************************************************************
  // ************************ CHANNEL D ***************************
  // **************************************************************

  def AccessAck(denied: Int, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleD = {
    // TODO: add checks for truncation
    new TLBundleD(params).Lit(
      _.opcode -> TLOpcodes.AccessAck.U,
      _.param -> 0.U,
      _.size -> size.U,
      _.source -> source.U,
      _.sink -> 0.U,
      _.denied -> denied.B,
      _.corrupt -> 0.B,
      _.data -> 0.U
    )
  }

  def AccessAck(denied: Int, source: Int = 0)(implicit params: TLBundleParameters): TLBundleD = {
    AccessAck(denied = denied, size = log2Ceil(params.dataBits/8), source = source)
  }

  def AccessAckData(data: BigInt, denied: Int, size: Int, source: Int)(implicit params: TLBundleParameters): TLBundleD = {
    // TODO: add checks for truncation
    new TLBundleD(params).Lit(
      _.opcode -> TLOpcodes.AccessAckData.U,
      _.param -> 0.U,
      _.size -> size.U,
      _.source -> source.U,
      _.sink -> 0.U,
      _.denied -> denied.B,
      _.corrupt -> 0.B,
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
      _.opcode -> TLOpcodes.HintAck.U,
      _.param -> 0.U,
      _.size -> size.U,
      _.source -> source.U,
      _.sink -> 0.U,
      _.denied -> denied.B,
      _.corrupt -> 0.B,
      _.data -> 0.U
    )
  }

  def HintAck(denied: Int, source: Int = 0)(implicit params: TLBundleParameters): TLBundleD = {
    HintAck(denied = denied, size = log2Ceil(params.dataBits/8), source = source)
  }

  def Grant(param: Int, denied: Int, size: Int, source: Int, sink: Int)(implicit params: TLBundleParameters): TLBundleD = {
    // TODO: add checks for truncation
    new TLBundleD(params).Lit(
      _.opcode -> TLOpcodes.Grant.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.sink -> sink.U,
      _.denied -> denied.B,
      _.corrupt -> 0.B,
      _.data -> 0.U
    )
  }

  def GrantData(param: Int, data: BigInt, denied: Int, size: Int, source: Int, sink: Int)(implicit params: TLBundleParameters): TLBundleD = {
    // TODO: add checks for truncation
    new TLBundleD(params).Lit(
      _.opcode -> TLOpcodes.GrantData.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.sink -> sink.U,
      _.denied -> denied.B,
      _.corrupt -> 0.B,
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
      _.opcode -> TLOpcodes.ReleaseAck.U,
      _.param -> 0.U,
      _.size -> size.U,
      _.source -> source.U,
      _.sink -> 0.U,
      _.denied -> 0.B,
      _.corrupt -> 0.B,
      _.data -> 0.U
    )
  }

  // **************************************************************
  // ************************ CHANNEL E ***************************
  // **************************************************************

  def GrantAck(sink: Int)(implicit params: TLBundleParameters): TLBundleE = {
    new TLBundleE(params).Lit(
      _.sink -> sink.U
    )
  }
}
