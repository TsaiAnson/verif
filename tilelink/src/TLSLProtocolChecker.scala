package verif

import chisel3._
import chisel3.util.log2Ceil
import freechips.rocketchip.tilelink._
import scala.collection.mutable.HashMap
import TLTransaction._
import TLUtils._
import SL._

trait TLMessageAP {
  // Channel A
  val IsGet = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.opcode.litValue() == TLOpcodes.Get; case _: TLBundleD => false}}, "AD: If Get Message") // AD Stands for "Supports Channel A and D"
  val IsPutFull = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.opcode.litValue() == TLOpcodes.PutFullData; case _: TLBundleD => false}}, "AD: If PutFullData Message")
  val IsPutPartial = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.opcode.litValue() == TLOpcodes.PutPartialData; case _: TLBundleD => false}}, "AD: If PutPartialData Message")
  val IsArith = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.opcode.litValue() == TLOpcodes.ArithmeticData; case _: TLBundleD => false}}, "AD: If ArithmeticData Message")
  val IsLogic = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.opcode.litValue() == TLOpcodes.LogicalData; case _: TLBundleD => false}}, "AD: If LogicalData Message")
  val IsHint = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.opcode.litValue() == TLOpcodes.Hint; case _: TLBundleD => false}}, "AD: If Hint Message")

  // Channel D
  val IsAccessAck = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case _: TLBundleA => false; case t: TLBundleD => t.opcode.litValue() == TLOpcodes.AccessAck}}, "AD: If AccessAck Message")
  val IsAccessAckData = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case _: TLBundleA => false; case t: TLBundleD => t.opcode.litValue() == TLOpcodes.AccessAckData}}, "AD: If AccessAckData Message")
  val IsHintAck = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
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
    t match {case t: TLBundleA => (t.address.litValue() & ((2 << t.size.litValue().toInt) - 1)) == 0; case _: TLBundleD => false}}, "AD: If Address is aligned to Size")
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
    t match {case t: TLBundleA => t.size.litValue() >= 0 && t.size.litValue() <= log2Ceil(maxTransfer); case _ => false}},
    "AD: If Size smaller than Max Transfer Size")
  def MaskWithinSize(beatBytes: Int) = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => if (t.size.litValue() > log2Ceil(beatBytes)) {(1 << (beatBytes + 1)) > t.mask.litValue()}
                                  else {(1 << (t.size.litValue().toInt + 1)) > t.mask.litValue()}; case _ => false}},
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
    t match {case _: TLBundleA => false; case t: TLBundleD => m.get.get(0).litValue() == t.source.litValue()}}, "AD: Check Data on Channel D")
}

trait TLMessageAPs extends TLMessageAP with TLStaticParameterAP with TLDynamicParameterAP {
  def GetAP(beatBytes: Int, maxTxSize: Int) = IsGet & ZeroParam & SizeWithinMaxTx(maxTxSize) & AlignedAddr & ContiguousMask & MaskWithinSize(beatBytes) & ZeroCorrupt
  def PutFullAP(beatBytes: Int, maxTxSize: Int) = IsPutFull & ZeroParam & SizeWithinMaxTx(maxTxSize) & AlignedAddr & ContiguousMask & MaskWithinSize(beatBytes)
  def PutPartialAP(beatBytes: Int, maxTxSize: Int) = IsPutPartial & ZeroParam & SizeWithinMaxTx(maxTxSize) & AlignedAddr & MaskWithinSize(beatBytes)
  def ArithAP(beatBytes: Int, maxTxSize: Int) = IsArith & ArithParam & SizeWithinMaxTx(maxTxSize) & AlignedAddr & ContiguousMask & MaskWithinSize(beatBytes)
  def LogicAP(beatBytes: Int, maxTxSize: Int) = IsLogic & LogicParam & SizeWithinMaxTx(maxTxSize) & AlignedAddr & ContiguousMask & MaskWithinSize(beatBytes)
  def HintAP(beatBytes: Int, maxTxSize: Int) = IsHint & HintParam & SizeWithinMaxTx(maxTxSize) & AlignedAddr & ContiguousMask & MaskWithinSize(beatBytes) & ZeroCorrupt
  def AccessAckAP(maxTxSize: Int) = IsAccessAck & ZeroParam & SizeWithinMaxTx(maxTxSize) & ZeroCorrupt
  def AccessAckDataAP(maxTxSize: Int) = IsAccessAck & ZeroParam & SizeWithinMaxTx(maxTxSize) & DeniedCorrupt
  def HintAckAP(maxTxSize: Int) = IsHintAck & ZeroParam & SizeWithinMaxTx(maxTxSize) & ZeroCorrupt
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

// Note: only supports up to bursts of 4, rest unchecked
class TLUProperties(beatBytes: Int) extends TLMessageAPs with TLModelingAPs  with BurstSizeAP {
  // Message Properties
  def GetProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt](IsGet, Implies, GetAP(beatBytes, maxTxSize))
  def PutPullProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt](IsPutFull, Implies, PutFullAP(beatBytes, maxTxSize))
  def PutPartialProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt](IsPutPartial, Implies, PutPartialAP(beatBytes, maxTxSize))
  def ArithProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt](IsArith, Implies, ArithAP(beatBytes, maxTxSize))
  def LogicProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt](IsLogic, Implies, LogicAP(beatBytes, maxTxSize))
  def HintProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt](IsHint, Implies, HintAP(beatBytes, maxTxSize))
  def AccessAckProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt](IsAccessAck, Implies, AccessAckAP(maxTxSize))
  def AccessAckDataProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt](IsAccessAckData, Implies, AccessAckDataAP(maxTxSize))
  def HintAckProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt](IsHintAck, Implies, HintAckAP(maxTxSize))

  // Handshake Properties (Message properties not checked here, see above)
  val GetDataSingleBeatHandshakeProperty = qProp[TLChannel, Int, UInt](IsGet & SaveSource & OneBeat(beatBytes), Implies, ###(1,-1), IsAccessAckData & CheckSource & CheckData)
  val PutFullSingleBeatDataHandshakeProperty = qProp[TLChannel, Int, UInt](IsPutFull & SaveSource & OneBeat(beatBytes), Implies, ###(1,-1), IsAccessAck & CheckSource)
  val PutPartialSingleBeatDataHandshakeProperty = qProp[TLChannel, Int, UInt](IsPutPartial & SaveSource & OneBeat(beatBytes), Implies, ###(1,-1), IsAccessAck & CheckSource)
  val ArithSingleBeatDataHandhsakeProperty = qProp[TLChannel, Int, UInt](IsArith & SaveSource & OneBeat(beatBytes), Implies, ###(1,-1), IsAccessAckData & CheckSource & CheckData)
  val LogicSingleBeatDataHandhsakeProperty = qProp[TLChannel, Int, UInt](IsLogic & SaveSource & OneBeat(beatBytes), Implies, ###(1,-1), IsAccessAckData & CheckSource & CheckData)

  val GetDataTwoBeatHandshakeProperty = qProp[TLChannel, Int, UInt](IsGet & SaveSource & TwoBeat(beatBytes), Implies, ###(1,-1), IsAccessAckData & CheckSource & CheckData, ###(1,-1), IsAccessAckData & CheckSource & CheckData)
  val PutFullTwoBeatDataHandshakeProperty = qProp[TLChannel, Int, UInt](IsPutFull & SaveSource & TwoBeat(beatBytes), Implies, ###(1,-1), IsAccessAck & CheckSource, ###(1,-1), IsAccessAck & CheckSource)
  val PutPartialTwoBeatDataHandshakeProperty = qProp[TLChannel, Int, UInt](IsPutPartial & SaveSource & TwoBeat(beatBytes), Implies, ###(1,-1), IsAccessAck & CheckSource, ###(1,-1), IsAccessAck & CheckSource)
  val ArithTwoBeatDataHandhsakeProperty = qProp[TLChannel, Int, UInt](IsArith & SaveSource & TwoBeat(beatBytes), Implies, ###(1,-1), IsAccessAckData & CheckSource & CheckData, ###(1,-1), IsAccessAckData & CheckSource & CheckData)
  val LogicTwoBeatDataHandhsakeProperty = qProp[TLChannel, Int, UInt](IsLogic & SaveSource & TwoBeat(beatBytes), Implies, ###(1,-1), IsAccessAckData & CheckSource & CheckData, ###(1,-1), IsAccessAckData & CheckSource & CheckData)

  val GetDataFourBeatHandshakeProperty = qProp[TLChannel, Int, UInt](IsGet & SaveSource & FourBeat(beatBytes), Implies, ###(1,-1), IsAccessAckData & CheckSource & CheckData,
    ###(1,-1), IsAccessAckData & CheckSource & CheckData,
    ###(1,-1), IsAccessAckData & CheckSource & CheckData,
    ###(1,-1), IsAccessAckData & CheckSource & CheckData)
  val PutFullFourBeatDataHandshakeProperty = qProp[TLChannel, Int, UInt](IsPutFull & SaveSource & FourBeat(beatBytes), Implies, ###(1,-1), IsAccessAck & CheckSource,
    ###(1,-1), IsAccessAck & CheckSource,
    ###(1,-1), IsAccessAck & CheckSource,
    ###(1,-1), IsAccessAck & CheckSource)
  val PutPartialFourBeatDataHandshakeProperty = qProp[TLChannel, Int, UInt](IsPutPartial & SaveSource & FourBeat(beatBytes), Implies, ###(1,-1), IsAccessAck & CheckSource,
    ###(1,-1), IsAccessAck & CheckSource,
    ###(1,-1), IsAccessAck & CheckSource,
    ###(1,-1), IsAccessAck & CheckSource)
  val ArithFourBeatDataHandhsakeProperty = qProp[TLChannel, Int, UInt](IsArith & SaveSource & FourBeat(beatBytes), Implies, ###(1,-1), IsAccessAckData & CheckSource & CheckData,
    ###(1,-1), IsAccessAckData & CheckSource & CheckData,
    ###(1,-1), IsAccessAckData & CheckSource & CheckData,
    ###(1,-1), IsAccessAckData & CheckSource & CheckData)
  val LogicFourBeatDataHandhsakeProperty = qProp[TLChannel, Int, UInt](IsLogic & SaveSource, Implies, ###(1,-1), IsAccessAckData & CheckSource & CheckData,
    ###(1,-1), IsAccessAckData & CheckSource & CheckData,
    ###(1,-1), IsAccessAckData & CheckSource & CheckData,
    ###(1,-1), IsAccessAckData & CheckSource & CheckData)

  val HintHandhsakeProperty = qProp[TLChannel, Int, UInt](IsHint & SaveSource, Implies, ###(1,-1), IsHintAck & CheckSource)

  def GetProperties(maxTxSize: Int): Seq[Property[TLChannel, Int, UInt]] = {
    var result = Seq(GetProperty(maxTxSize), GetDataSingleBeatHandshakeProperty)
    if (maxTxSize > beatBytes) {
      result = result :+ GetDataTwoBeatHandshakeProperty
    }
    if (maxTxSize > beatBytes * 2) {
      result = result :+ GetDataFourBeatHandshakeProperty
    }
    result
  }

  def PutFullProperties(maxTxSize: Int): Seq[Property[TLChannel, Int, UInt]] = {
    var result = Seq(PutPullProperty(maxTxSize), PutFullSingleBeatDataHandshakeProperty)
    if (maxTxSize > beatBytes) {
      result = result :+ PutFullTwoBeatDataHandshakeProperty
    }
    if (maxTxSize > beatBytes * 2) {
      result = result :+ PutFullFourBeatDataHandshakeProperty
    }
    result
  }

  def PutPartialProperties(maxTxSize: Int): Seq[Property[TLChannel, Int, UInt]] = {
    var result = Seq(PutPartialProperty(maxTxSize), PutPartialSingleBeatDataHandshakeProperty)
    if (maxTxSize > beatBytes) {
      result = result :+ PutPartialTwoBeatDataHandshakeProperty
    }
    if (maxTxSize > beatBytes * 2) {
      result = result :+ PutPartialFourBeatDataHandshakeProperty
    }
    result
  }

  def ArithProperties(maxTxSize: Int): Seq[Property[TLChannel, Int, UInt]] = {
    var result = Seq(ArithProperty(maxTxSize), ArithSingleBeatDataHandhsakeProperty)
    if (maxTxSize > beatBytes) {
      result = result :+ ArithTwoBeatDataHandhsakeProperty
    }
    if (maxTxSize > beatBytes * 2) {
      result = result :+ ArithFourBeatDataHandhsakeProperty
    }
    result
  }

  def LogicProperties(maxTxSize: Int): Seq[Property[TLChannel, Int, UInt]] = {
    var result = Seq(LogicProperty(maxTxSize), LogicSingleBeatDataHandhsakeProperty)
    if (maxTxSize > beatBytes) {
      result = result :+ LogicTwoBeatDataHandhsakeProperty
    }
    if (maxTxSize > beatBytes * 2) {
      result = result :+ LogicFourBeatDataHandhsakeProperty
    }
    result
  }

  def HintProperties(maxTxSize: Int) = Seq(
    HintProperty(maxTxSize),
    HintAckProperty(maxTxSize),
    HintHandhsakeProperty,
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
  def check(txns: Seq[TLChannel], memModel: SLMemoryModel[TLChannel,UInt]): Boolean = {
    val memoryStates = memModel.model(txns)

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
