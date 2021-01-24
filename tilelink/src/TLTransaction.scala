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

  def getSize(beats: Int)(implicit p: TLBundleParameters): Int = {
    assert(p.dataBits % 8 == 0)
    assert(beats > 0 && (beats & (beats - 1)) == 0) // assert beats is a power of 2
    log2Ceil(p.dataBits/8 * beats)
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
  def Get(addr: BigInt, source: Int = 0)(implicit params: TLBundleParameters): TLBundleA = {
    Get(addr = addr, size = getSize(1), mask = fullMask, source = source)
  }

  def Put(addr: BigInt, data: BigInt, mask: Int, size: Int, source: Int, partialHint: Boolean)(implicit params: TLBundleParameters): TLBundleA = {
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
  def Put(addr: BigInt, data: BigInt, mask: Int, source: Int)(implicit params: TLBundleParameters): TLBundleA = {
    Put(addr = addr, data = data, mask = mask, size = getSize(1), source = source, partialHint = false)
  }

  def Put(addr: BigInt, data: BigInt, source: Int = 0)(implicit params: TLBundleParameters): TLBundleA = {
    Put(addr = addr, data = data, mask = fullMask, size = getSize(1), source = source, partialHint = false)
  }

  // No masks assume PutFull
  def PutBurst(addr: BigInt, data: Seq[BigInt], source: Int)(implicit params: TLBundleParameters): Seq[TLBundleA] = {
    data.map {
      (d: BigInt) => Put(addr = addr, data = d, mask = fullMask, size = getSize(data.length), source = source, partialHint = false)
    }
  }

  // Masks assume PutPartial
  def PutBurst(addr: BigInt, data: Seq[BigInt], mask: Seq[Int], source: Int)(implicit params: TLBundleParameters): Seq[TLBundleA] = {
    (mask zip data).map {
      case (m, d) => Put(addr = addr, data = d, mask = m, size = getSize(data.length), source = source, partialHint = true)
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
    Arith(param = param, addr = addr, data = data, mask = fullMask, size = getSize(1), source = source)
  }

  def ArithBurst(param: Int, addr: BigInt, data: Seq[BigInt], source: Int = 0)(implicit params: TLBundleParameters): Seq[TLBundleA] = {
    data.map {
      (d: BigInt) => Arith(param, addr, d, fullMask, getSize(data.length), source)
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
    Logic(param = param, addr = addr, data = data, mask = fullMask, size = getSize(1), source = source)
  }

  def LogicBurst(param: Int, addr: BigInt, data: Seq[BigInt], source: Int = 0)(implicit params: TLBundleParameters): Seq[TLBundleA] = {
    data.map {
      (d: BigInt) => Logic(param, addr, d, fullMask, getSize(data.length), source)
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
    Intent(param = param, addr = addr, mask = fullMask, size = getSize(1), source = source)
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
      _.opcode -> TLOpcodes.ProbeAck.U,
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
      (d: BigInt) => ProbeAckData(param, addr, d, getSize(data.length), source)
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
      (d: BigInt) => ReleaseData(param, addr, d, getSize(data.length), source)
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
    AccessAck(denied = denied, size = getSize(1), source = source)
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
      _.corrupt -> denied.B, // Corrupt MUST be high if Denied
      _.data -> data.U
    )
  }

  def AccessAckData(data: BigInt, denied: Int, source: Int = 0)(implicit params: TLBundleParameters): TLBundleD = {
    AccessAckData(data = data, denied = denied, size = getSize(1), source = source)
  }

  def AccessAckDataBurst(data: Seq[BigInt], denied: Int, source: Int = 0)(implicit params: TLBundleParameters): Seq[TLBundleD] = {
    data.map {
      (d: BigInt) => AccessAckData(data = d, denied = denied, size = getSize(data.length), source = source)
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
    HintAck(denied = denied, size = getSize(1), source = source)
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
    GrantData(param = param, data = data, denied = denied, size = getSize(1), source = source, sink = sink)
  }

  def GrantDataBurst(param: Int, data: Seq[BigInt], denied: Int, sink: Int, source: Int = 0)(implicit params: TLBundleParameters): Seq[TLBundleD] = {
    data.map {
      (d: BigInt) => GrantData(param = param, data = d, denied = denied, size = getSize(data.length), source = source, sink = sink)
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

  case class Mismatch(idx: Int, field: String, expected: chisel3.Data, actual:chisel3.Data) {
    override def toString: String = f"MISMATCH Index: $idx%05d Field: $field Expected: ${expected.litValue()}%#x Got: ${actual.litValue()}%#x"
  }

  def findMismatch(idx: Int, dut: TLBundleD, gold: TLBundleD): Seq[Mismatch] = {
    if (dut.opcode.litValue() != gold.opcode.litValue()) {
      Seq(Mismatch(idx, dut.opcode.name, gold.opcode, dut.opcode))
    } else {
      val fieldsToCompare = Seq[TLBundleD => chisel3.UInt](t => t.param, t => t.size, t => t.source, t => t.sink, t => t.denied)
      dut.opcode.litValue().toInt match {
        case TLOpcodes.AccessAck =>
          fieldsToCompare.foldLeft(Seq.empty[Mismatch]) {
            case (mismatches, fieldFn) =>
              if (fieldFn(dut).litValue() != fieldFn(gold).litValue()) {
                mismatches :+ Mismatch(idx, fieldFn(dut).name, fieldFn(gold), fieldFn(dut))
              } else {
                mismatches
              }
          }
        case TLOpcodes.AccessAckData => // TODO: duplicated code
          val dataFields = fieldsToCompare ++ Seq[TLBundleD => chisel3.UInt](t => t.data, t => t.corrupt)
          dataFields.foldLeft(Seq.empty[Mismatch]) {
            case (mismatches, fieldFn) =>
              if (fieldFn(dut).litValue() != fieldFn(gold).litValue()) {
                mismatches :+ Mismatch(idx, fieldFn(dut).name, fieldFn(gold), fieldFn(dut))
              } else {
                mismatches
              }
          }
        case _ => ???
      }
    }
  }

  def equalsTL(dut: Seq[TLBundleD], gold: Seq[TLBundleD]): Seq[Mismatch] = {
    val directMismatches = dut.zip(gold).zipWithIndex.foldLeft(Seq.empty[Mismatch]) {
      case (mismatches, ((dutTx, goldTx), idx)) =>
        mismatches ++ findMismatch(idx, dutTx, goldTx)
    }
    val lengthMismatches = if (dut.length > gold.length) {
      dut.slice(gold.length, dut.length).zipWithIndex.map {
        case (dutTx, idx) =>
          Mismatch(idx + gold.length, "Unmatched DUT TX", 0.U, dutTx) // TODO: expected field is meaningless here
      }
    } else {
      gold.slice(dut.length, gold.length).zipWithIndex.map {
        case (goldTx, idx) =>
          Mismatch(idx + dut.length, "Unmatched Gold TX", 0.U, goldTx) // TODO: expected field is meaningless here
      }
    }
    directMismatches ++ lengthMismatches
  }
}
