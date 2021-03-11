package verif

import chisel3._
import chisel3.util.log2Ceil
import freechips.rocketchip.tilelink._
import scala.collection.mutable.HashMap
import TLTransaction._
import SL._

trait TLMessageAP {
  // Channel A
  val IsGetOp = qAP({ (t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.opcode.litValue() == TLOpcodes.Get; case _: TLBundleD => false}}, "AD: If Get Message") // AD Stands for "Supports Channel A and D"
  val IsPutFullOp = qAP({ (t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.opcode.litValue() == TLOpcodes.PutFullData; case _: TLBundleD => false}}, "AD: If PutFullData Message")
  val IsPutPartialOp = qAP({ (t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.opcode.litValue() == TLOpcodes.PutPartialData; case _: TLBundleD => false}}, "AD: If PutPartialData Message")
  val IsArithOp = qAP({ (t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.opcode.litValue() == TLOpcodes.ArithmeticData; case _: TLBundleD => false}}, "AD: If ArithmeticData Message")
  val IsLogicOp = qAP({ (t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.opcode.litValue() == TLOpcodes.LogicalData; case _: TLBundleD => false}}, "AD: If LogicalData Message")
  val IsHintOp = qAP({ (t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.opcode.litValue() == TLOpcodes.Hint; case _: TLBundleD => false}}, "AD: If Hint Message")

  // Channel D
  val IsAccessAckOp = qAP({ (t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case _: TLBundleA => false; case t: TLBundleD => t.opcode.litValue() == TLOpcodes.AccessAck}}, "AD: If AccessAck Message")
  val IsAccessAckDataOp = qAP({ (t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case _: TLBundleA => false; case t: TLBundleD => t.opcode.litValue() == TLOpcodes.AccessAckData}}, "AD: If AccessAckData Message")
  val IsHintAckOp = qAP({ (t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case _: TLBundleA => false; case t: TLBundleD => t.opcode.litValue() == TLOpcodes.HintAck}}, "AD: If HintAck Message")
}

trait TLStaticParameterAP {
  val ZeroParam = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.param.litValue() == 0; case t: TLBundleD => t.param.litValue() == 0}}, "AD: If param is 0")
  val ArithParam = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.param.litValue() >= 0 && t.param.litValue() <= 4; case _: TLBundleD => false}}, "AD: If param is legal Arith")
  val LogicParam = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.param.litValue() >= 0 && t.param.litValue() <= 3; case _: TLBundleD => false}}, "AD: If param is legal Logical")
  val HintParam = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.param.litValue() >= 0 && t.param.litValue() <= 1; case _: TLBundleD => false}}, "AD: If param is legal Hint")
  val AlignedAddr = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => (t.address.litValue() & ((1 << t.size.litValue().toInt) - 1)) == 0; case _: TLBundleD => false}}, "AD: If Address is aligned to Size")
  val ContiguousMask = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => (t.mask.litValue() & (t.mask.litValue() + 1)) == 0; case _: TLBundleD => false}}, "AD: If Mask is Contiguous")
  val ZeroCorrupt = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.corrupt.litValue() == 0; case t: TLBundleD => t.corrupt.litValue() == 0}}, "AD: If corrupt is 0")
  val DeniedCorrupt = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case _: TLBundleA => false; case t: TLBundleD => if (t.denied.litToBoolean) {t.corrupt.litToBoolean} else {true}}}, "AD: If Denied, Corrupt high")
}

// Requires parameters
trait TLDynamicParameterAP {
  def SizeWithinMaxTx(maxTransfer: Int) = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.size.litValue() >= 0 && t.size.litValue() <= log2Ceil(maxTransfer); case t: TLBundleD =>
      t.size.litValue() >= 0 && t.size.litValue() <= log2Ceil(maxTransfer)}},
    "AD: If Size smaller than Max Transfer Size")
  def MaskAllHigh(beatBytes: Int) = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => if (t.size.litValue() > log2Ceil(beatBytes)) {(1 << beatBytes) - 1 == t.mask.litValue()}
    else {(1 << (1 << t.size.litValue().toInt)) - 1 == t.mask.litValue()}; case _ => false}},
    "AD: If Mask All High")
  def MaskWithinSize(beatBytes: Int) = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => if (t.size.litValue() > log2Ceil(beatBytes)) {(1 << beatBytes) > t.mask.litValue()}
                                  else {(1 << (1 << t.size.litValue().toInt)) > t.mask.litValue()}; case _ => false}},
    "AD: If Mask within Size")
}

trait TLModelingAPs {
  val SaveSource = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => {h("source") = t.source.litValue().toInt; true}; case _: TLBundleD => false}}, "AD: Save Source on Channel A")
  val CheckSource = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case _: TLBundleA => false; case t: TLBundleD => h("source") == t.source.litValue()}}, "AD: Check Source on Channel D")
  val SaveSize = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => {h("size") = t.size.litValue().toInt; true}; case _: TLBundleD => false}}, "AD: Save Size on Channel A")
  val CheckSize = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case _: TLBundleA => false; case t: TLBundleD => h("size") == t.size.litValue()}}, "AD: Check Size on Channel D")
  val CheckData = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case _: TLBundleA => false; case t: TLBundleD => if (m.isDefined) m.get.get(0).litValue() == t.data.litValue() else true}}, "AD: Check Data on Channel D")
}

trait TLMessageAPs extends TLMessageAP with TLStaticParameterAP with TLDynamicParameterAP {
  def GetAP(beatBytes: Int, maxTxSize: Int) = IsGetOp & ZeroParam & SizeWithinMaxTx(maxTxSize) & AlignedAddr & ContiguousMask & MaskWithinSize(beatBytes) & MaskAllHigh(beatBytes) & ZeroCorrupt
  def PutFullAP(beatBytes: Int, maxTxSize: Int) = IsPutFullOp & ZeroParam & SizeWithinMaxTx(maxTxSize) & AlignedAddr & ContiguousMask & MaskWithinSize(beatBytes) & MaskAllHigh(beatBytes)
  def PutPartialAP(beatBytes: Int, maxTxSize: Int) = IsPutPartialOp & ZeroParam & SizeWithinMaxTx(maxTxSize) & AlignedAddr & MaskWithinSize(beatBytes)
  def ArithAP(beatBytes: Int, maxTxSize: Int) = IsArithOp & ArithParam & SizeWithinMaxTx(maxTxSize) & AlignedAddr & ContiguousMask & MaskWithinSize(beatBytes) & MaskAllHigh(beatBytes)
  def LogicAP(beatBytes: Int, maxTxSize: Int) = IsLogicOp & LogicParam & SizeWithinMaxTx(maxTxSize) & AlignedAddr & ContiguousMask & MaskWithinSize(beatBytes) & MaskAllHigh(beatBytes)
  def HintAP(beatBytes: Int, maxTxSize: Int) = IsHintOp & HintParam & SizeWithinMaxTx(maxTxSize) & AlignedAddr & ContiguousMask & MaskWithinSize(beatBytes) & MaskAllHigh(beatBytes) & MaskAllHigh(beatBytes) & ZeroCorrupt
  def AccessAckAP(maxTxSize: Int) = IsAccessAckOp & ZeroParam & SizeWithinMaxTx(maxTxSize) & ZeroCorrupt
  def AccessAckDataAP(maxTxSize: Int) = IsAccessAckDataOp & ZeroParam & SizeWithinMaxTx(maxTxSize) & DeniedCorrupt
  def HintAckAP(maxTxSize: Int) = IsHintAckOp & ZeroParam & SizeWithinMaxTx(maxTxSize) & ZeroCorrupt
}

// Temporary Size APs for different bursts: Up to 4 beats
trait BurstSizeAP {
  def OneBeat(beatBytes: Int) = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.size.litValue().toInt <= log2Ceil(beatBytes); case _: TLBundleD => false}}, "AD: If Size is Single Beat")
  def TwoBeat(beatBytes: Int) = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.size.litValue().toInt == log2Ceil(beatBytes) + 1; case _: TLBundleD => false}}, "AD: If Size is Two Beats")
  def FourBeat(beatBytes: Int) = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.size.litValue().toInt == log2Ceil(beatBytes) + 2; case _: TLBundleD => false}}, "AD: If Size is Four Beats")
}

// Other Utility APs
trait MiscAP {
  // Returns false always (used in properties that SHOULD NOT be triggered, e.g. unexpected response message
  def AlwaysFalse  = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case _ => false; case _: TLBundleD => false}}, "AD: Always False")
}

// Note: only supports up to bursts of 4, rest unchecked
class TLUProperties(beatBytes: Int) extends TLMessageAPs with TLModelingAPs  with BurstSizeAP with MiscAP {
  // Message Properties
  def GetProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt]("Correct Get Fields", IsGetOp, Implies, GetAP(beatBytes, maxTxSize))
  def PutPullProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt]("Correct PutFull Fields",IsPutFullOp, Implies, PutFullAP(beatBytes, maxTxSize))
  def PutPartialProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt]("Correct PutPartial Fields",IsPutPartialOp, Implies, PutPartialAP(beatBytes, maxTxSize))
  def ArithProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt]("Correct Arith Fields",IsArithOp, Implies, ArithAP(beatBytes, maxTxSize))
  def LogicProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt]("Correct Logic Fields",IsLogicOp, Implies, LogicAP(beatBytes, maxTxSize))
  def HintProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt]("Correct Hint Fields",IsHintOp, Implies, HintAP(beatBytes, maxTxSize))
  def AccessAckProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt]("Correct AccessAck Fields",IsAccessAckOp, Implies, AccessAckAP(maxTxSize))
  def AccessAckDataProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt]("Correct AccessAckData Fields",IsAccessAckDataOp, Implies, AccessAckDataAP(maxTxSize))
  def HintAckProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt]("Correct HintAck Fields",IsHintAckOp, Implies, HintAckAP(maxTxSize))

  // Handshake Properties (Message properties not checked here, see above)
  // Defining helper sequences
  val GetOneBeatSequence = qSeq[TLChannel, Int, UInt](IsGetOp & SaveSource & SaveSize & OneBeat(beatBytes))
  val PutFullOneBeatSequence = qSeq[TLChannel, Int, UInt](IsPutFullOp & SaveSource & SaveSize & OneBeat(beatBytes))
  val PutPartialOneBeatSequence = qSeq[TLChannel, Int, UInt](IsPutPartialOp & SaveSource & SaveSize & OneBeat(beatBytes))
  val ArithOneBeatSequence = qSeq[TLChannel, Int, UInt](IsArithOp & SaveSource & SaveSize & OneBeat(beatBytes))
  val LogicOneBeatSequence = qSeq[TLChannel, Int, UInt](IsLogicOp & SaveSource & SaveSize & OneBeat(beatBytes))

  val GetTwoBeatSequence = qSeq[TLChannel, Int, UInt](IsGetOp & SaveSource & SaveSize & TwoBeat(beatBytes))
  val PutFullTwoBeatSequence = qSeq[TLChannel, Int, UInt](IsPutFullOp & SaveSource & SaveSize & TwoBeat(beatBytes))
  val PutPartialTwoBeatSequence = qSeq[TLChannel, Int, UInt](IsPutPartialOp & SaveSource & SaveSize & TwoBeat(beatBytes))
  val ArithTwoBeatSequence = qSeq[TLChannel, Int, UInt](IsArithOp & SaveSource & SaveSize & TwoBeat(beatBytes))
  val LogicTwoBeatSequence = qSeq[TLChannel, Int, UInt](IsLogicOp & SaveSource & SaveSize & TwoBeat(beatBytes))

  val GetFourBeatSequence = qSeq[TLChannel, Int, UInt](IsGetOp & SaveSource & SaveSize & FourBeat(beatBytes))
  val PutFullFourBeatSequence = qSeq[TLChannel, Int, UInt](IsPutFullOp & SaveSource & SaveSize & FourBeat(beatBytes))
  val PutPartialFourBeatSequence = qSeq[TLChannel, Int, UInt](IsPutPartialOp & SaveSource & SaveSize & FourBeat(beatBytes))
  val ArithFourBeatSequence = qSeq[TLChannel, Int, UInt](IsArithOp & SaveSource & SaveSize & FourBeat(beatBytes))
  val LogicFourBeatSequence = qSeq[TLChannel, Int, UInt](IsLogicOp & SaveSource & SaveSize & FourBeat(beatBytes))

  val AccessAckCheckSequence = qSeq[TLChannel, Int, UInt](###(1, -1), IsAccessAckOp & CheckSource & CheckSize)
  val AccessAckDataCheckSequence = qSeq[TLChannel, Int, UInt](###(1, -1), IsAccessAckDataOp & CheckSource & CheckSize & CheckData)

  // Handshake Properties
  val GetDataOneBeatHandshakeProperty = qProp[TLChannel, Int, UInt]("OneBeat Get", GetOneBeatSequence + Implies + ###(1, -1) + AccessAckDataCheckSequence)
  val PutFullOneBeatDataHandshakeProperty = qProp[TLChannel, Int, UInt]("OneBeat PutFull", PutFullOneBeatSequence + Implies + ###(1, -1) + AccessAckCheckSequence)
  val PutPartialOneBeatDataHandshakeProperty = qProp[TLChannel, Int, UInt]("OneBeat PutPartial", PutPartialOneBeatSequence + Implies + ###(1, -1) + AccessAckCheckSequence)
  val ArithOneBeatDataHandshakeProperty = qProp[TLChannel, Int, UInt]("OneBeat Arith", ArithOneBeatSequence + Implies + ###(1, -1) + AccessAckDataCheckSequence)
  val LogicOneBeatDataHandshakeProperty = qProp[TLChannel, Int, UInt]("OneBeat Logic", LogicOneBeatSequence + Implies + ###(1, -1) + AccessAckDataCheckSequence)

  val GetDataTwoBeatHandshakeProperty = qProp[TLChannel, Int, UInt]("TwoBeat Get", GetTwoBeatSequence + Implies + (AccessAckDataCheckSequence * 2))
  val PutFullTwoBeatDataHandshakeProperty = qProp[TLChannel, Int, UInt]("TwoBeat PutFull", (PutFullTwoBeatSequence + Implies + ###(1, -1)) * 2 + AccessAckCheckSequence)
  val PutPartialTwoBeatDataHandshakeProperty = qProp[TLChannel, Int, UInt]("TwoBeat PutPartial", (PutPartialTwoBeatSequence + Implies + ###(1, -1)) * 2 + AccessAckCheckSequence)
  val ArithTwoBeatDataHandshakeProperty = qProp[TLChannel, Int, UInt]("TwoBeat Arith", (ArithTwoBeatSequence + Implies + ###(1, -1)) * 2 + (AccessAckDataCheckSequence * 2))
  val LogicTwoBeatDataHandshakeProperty = qProp[TLChannel, Int, UInt]("TwoBeat Logic", (LogicTwoBeatSequence + Implies + ###(1, -1)) * 2 + (AccessAckDataCheckSequence * 2))

  val GetDataFourBeatHandshakeProperty = qProp[TLChannel, Int, UInt]("FourBeat Get", GetFourBeatSequence + Implies + (AccessAckDataCheckSequence * 4))
  val PutFullFourBeatDataHandshakeProperty = qProp[TLChannel, Int, UInt]("FourBeat PutFull", (PutFullFourBeatSequence + Implies + ###(1, -1)) * 4 + AccessAckCheckSequence)
  val PutPartialFourBeatDataHandshakeProperty = qProp[TLChannel, Int, UInt]("FourBeat PutPartial", (PutPartialFourBeatSequence + Implies + ###(1, -1)) * 4 + AccessAckCheckSequence)
  val ArithFourBeatDataHandshakeProperty = qProp[TLChannel, Int, UInt]("FourBeat Arith", (ArithFourBeatSequence + Implies + ###(1, -1)) * 4 + ( AccessAckDataCheckSequence * 4))
  val LogicFourBeatDataHandshakeProperty = qProp[TLChannel, Int, UInt]("FourBeat Logic", (LogicFourBeatSequence + Implies + ###(1, -1)) * 4 + (AccessAckDataCheckSequence * 4))

  val HintHandshakeProperty = qProp[TLChannel, Int, UInt]("Hint Handshake", IsHintOp & SaveSource, Implies, ###(1,-1), IsHintAckOp & CheckSource)

  // Helper methods to get properties
  def GetProperties(maxTxSize: Int): Seq[Property[TLChannel, Int, UInt]] = {
    var result = Seq(GetProperty(maxTxSize), GetDataOneBeatHandshakeProperty)
    if (maxTxSize > beatBytes) {
      result = result :+ GetDataTwoBeatHandshakeProperty
    }
    if (maxTxSize > beatBytes * 2) {
      result = result :+ GetDataFourBeatHandshakeProperty
    }
    result
  }

  def PutFullProperties(maxTxSize: Int): Seq[Property[TLChannel, Int, UInt]] = {
    var result = Seq(PutPullProperty(maxTxSize), PutFullOneBeatDataHandshakeProperty)
    if (maxTxSize > beatBytes) {
      result = result :+ PutFullTwoBeatDataHandshakeProperty
    }
    if (maxTxSize > beatBytes * 2) {
      result = result :+ PutFullFourBeatDataHandshakeProperty
    }
    result
  }

  def PutPartialProperties(maxTxSize: Int): Seq[Property[TLChannel, Int, UInt]] = {
    var result = Seq(PutPartialProperty(maxTxSize), PutPartialOneBeatDataHandshakeProperty)
    if (maxTxSize > beatBytes) {
      result = result :+ PutPartialTwoBeatDataHandshakeProperty
    }
    if (maxTxSize > beatBytes * 2) {
      result = result :+ PutPartialFourBeatDataHandshakeProperty
    }
    result
  }

  def ArithProperties(maxTxSize: Int): Seq[Property[TLChannel, Int, UInt]] = {
    var result = Seq(ArithProperty(maxTxSize), ArithOneBeatDataHandshakeProperty)
    if (maxTxSize > beatBytes) {
      result = result :+ ArithTwoBeatDataHandshakeProperty
    }
    if (maxTxSize > beatBytes * 2) {
      result = result :+ ArithFourBeatDataHandshakeProperty
    }
    result
  }

  def LogicProperties(maxTxSize: Int): Seq[Property[TLChannel, Int, UInt]] = {
    var result = Seq(LogicProperty(maxTxSize), LogicOneBeatDataHandshakeProperty)
    if (maxTxSize > beatBytes) {
      result = result :+ LogicTwoBeatDataHandshakeProperty
    }
    if (maxTxSize > beatBytes * 2) {
      result = result :+ LogicFourBeatDataHandshakeProperty
    }
    result
  }

  def HintProperties(maxTxSize: Int) = Seq(
    HintProperty(maxTxSize),
    HintAckProperty(maxTxSize),
    HintHandshakeProperty,
  )

  def CommonProperties(maxTxSize: Int) = Seq(
    AccessAckProperty(maxTxSize),
    AccessAckDataProperty(maxTxSize),
  )
}

// NOTE: Currently only supports TL-U
class TLSLProtocolChecker(mparam: TLMasterPortParameters, sparam: TLSlavePortParameters) {
  val bparam = TLBundleParameters(mparam, sparam)
  // PB for Property Bank
  val pb = new TLUProperties(sparam.beatBytes)
  // Properties to check
  var checkedProperties: Seq[Property[TLChannel, Int, UInt]] = Seq()

  // Populating properties
  if (sparam.allSupportGet) {
    checkedProperties = checkedProperties ++ pb.GetProperties(sparam.allSupportGet.max)
  }
  if (sparam.allSupportPutFull) {
    checkedProperties = checkedProperties ++ pb.PutFullProperties(sparam.allSupportPutFull.max)
  }
  if (sparam.allSupportPutPartial) {
    checkedProperties = checkedProperties ++ pb.PutPartialProperties(sparam.allSupportPutPartial.max)
  }
  if (sparam.allSupportArithmetic) {
    checkedProperties = checkedProperties ++ pb.ArithProperties(sparam.allSupportArithmetic.max)
  }
  if (sparam.allSupportLogical) {
    checkedProperties = checkedProperties ++ pb.LogicProperties(sparam.allSupportLogical.max)
  }
  if (sparam.allSupportHint) {
    checkedProperties = checkedProperties ++ pb.HintProperties(sparam.allSupportLogical.max)
  }
  if (checkedProperties.nonEmpty) {
    checkedProperties = checkedProperties ++ pb.CommonProperties(sparam.maxTransfer)
  }

  // Assumes complete transaction trace
  def check(txns: Seq[TLChannel], memModel: Option[SLMemoryModel[TLChannel,UInt]] = None): Boolean = {
    val memoryStates = if (memModel.isDefined) memModel.get.model(txns) else Seq.fill(txns.length)(None)

    var result = true
    for (property <- checkedProperties) {
      val temp = property.check(txns, memoryStates)
      if (!temp) println(s"Property failed: $property")
      result &= temp
    }
    if (!result) println(s"One or more properties failed. Please check the above log.")
    result
  }
}
