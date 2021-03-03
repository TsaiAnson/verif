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
    t match {case t: TLBundleA => t.opcode.litValue() == TLOpcodes.Get; case _: TLBundleD => false}}, "AD: If Get Message")
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
  val SizePositive = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.size.litValue() >= 0; case _: TLBundleD => false}}, "AD: If Size is positive")
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
  def SizeWithinBeatUL(beatBytes: Int) = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.size.litValue() >= 0 && t.size.litValue() <= log2Ceil(beatBytes); case t: TLBundleD => t.param.litValue() == 0}},
    "AD: If Size within BeatBytes")
  def MaskWithinSize(beatBytes: Int) = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => if (t.size.litValue() > log2Ceil(beatBytes)) {(1 << (beatBytes + 1)) > t.mask.litValue()}
                                  else {(1 << (t.size.litValue().toInt + 1)) > t.mask.litValue()}; case t: TLBundleD => t.param.litValue() == 0}},
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
  // TL-UL
  def GetULAP(beatBytes: Int) = IsGet & ZeroParam & SizePositive & SizeWithinBeatUL(beatBytes) & AlignedAddr &
    ContiguousMask & MaskWithinSize(beatBytes) & ZeroCorrupt
  def PutFullULAP(beatBytes: Int)  = IsPutFull & ZeroParam & SizePositive & SizeWithinBeatUL(beatBytes) & AlignedAddr &
    ContiguousMask & MaskWithinSize(beatBytes)
  def PutPartialULAP(beatBytes: Int)  = IsPutPartial & ZeroParam & SizePositive & SizeWithinBeatUL(beatBytes) & AlignedAddr & MaskWithinSize(beatBytes)
  def AccessAckULAP(beatBytes: Int)  = IsAccessAck & ZeroParam & SizePositive & SizeWithinBeatUL(beatBytes) & ZeroCorrupt
  def AccessAckDataULAP(beatBytes: Int)  = IsAccessAck & ZeroParam & SizePositive & SizeWithinBeatUL(beatBytes) & DeniedCorrupt

  // TL-UH
  def GetAP(beatBytes: Int) = IsGet & ZeroParam & SizePositive & AlignedAddr & ContiguousMask & MaskWithinSize(beatBytes) & ZeroCorrupt
  def PutFullAP(beatBytes: Int) = IsPutFull & ZeroParam & SizePositive & AlignedAddr & ContiguousMask & MaskWithinSize(beatBytes)
  def PutPartialAP(beatBytes: Int)  = IsPutPartial & ZeroParam & SizePositive & AlignedAddr & MaskWithinSize(beatBytes)
  def ArithAP(beatBytes: Int) = IsArith & ArithParam & SizePositive & AlignedAddr & ContiguousMask & MaskWithinSize(beatBytes)
  def LogicAP(beatBytes: Int) = IsLogic & LogicParam & SizePositive & AlignedAddr & ContiguousMask & MaskWithinSize(beatBytes)
  def HintAP(beatBytes: Int) = IsHint & HintParam & SizePositive & AlignedAddr & ContiguousMask & MaskWithinSize(beatBytes) & ZeroCorrupt
  def AccessAckAP(beatBytes: Int)  = IsAccessAck & ZeroParam & SizePositive & ZeroCorrupt
  def AccessAckDataAP(beatBytes: Int)  = IsAccessAck & ZeroParam & SizePositive & DeniedCorrupt
  def HintAckAP(beatBytes: Int)  = IsHintAck & ZeroParam & SizePositive & ZeroCorrupt
}

class TLULProperties(beatBytes: Int) extends TLMessageAPs with TLModelingAPs {
  // Message Properties
  val GetULProperty = qProp[TLChannel, Int, UInt](IsGet, Implies, GetULAP(beatBytes))
  val PutPullULProperty = qProp[TLChannel, Int, UInt](IsPutFull, Implies, PutFullULAP(beatBytes))
  val PutPartialULProperty = qProp[TLChannel, Int, UInt](IsPutPartial, Implies, PutPartialULAP(beatBytes))
  val AccessAckULProperty = qProp[TLChannel, Int, UInt](IsAccessAck, Implies, AccessAckULAP(beatBytes))
  val AccessAckDataULProperty = qProp[TLChannel, Int, UInt](IsAccessAckData, Implies, AccessAckDataULAP(beatBytes))

  // Handshake Properties (Message properties not checked here, see above)
  val GetDataHandshakeULProperty = qProp[TLChannel, Int, UInt](IsGet & SaveSource & SaveSize, Implies, ###(1,-1), IsAccessAckData & CheckSource & CheckSize & CheckData)
  val PutFullDataHandshakeULProperty = qProp[TLChannel, Int, UInt](IsPutFull & SaveSource & SaveSize, Implies, ###(1,-1), IsAccessAck & CheckSource & CheckSize)
  val PutPartialDataHandshakeULProperty = qProp[TLChannel, Int, UInt](IsPutPartial & SaveSource & SaveSize, Implies, ###(1,-1), IsAccessAck & CheckSource & CheckSize)

  val allProperties = Seq(
    GetULProperty,
    PutPullULProperty,
    PutPartialULProperty,
    AccessAckULProperty,
    AccessAckDataULProperty,
    GetDataHandshakeULProperty,
    PutFullDataHandshakeULProperty,
    PutPartialDataHandshakeULProperty
  )
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
class TLUHProperties(beatBytes: Int) extends TLMessageAPs with TLModelingAPs with BurstSizeAP {
  // Message Properties
  val GetProperty = qProp[TLChannel, Int, UInt](IsGet, Implies, GetAP(beatBytes))
  val PutPullProperty = qProp[TLChannel, Int, UInt](IsPutFull, Implies, PutFullAP(beatBytes))
  val PutPartialProperty = qProp[TLChannel, Int, UInt](IsPutPartial, Implies, PutPartialAP(beatBytes))
  val ArithmeticProperty = qProp[TLChannel, Int, UInt](IsArith, Implies, ArithAP(beatBytes))
  val LogicalProperty = qProp[TLChannel, Int, UInt](IsLogic, Implies, LogicAP(beatBytes))
  val HintProperty = qProp[TLChannel, Int, UInt](IsHint, Implies, HintAP(beatBytes))
  val AccessAckProperty = qProp[TLChannel, Int, UInt](IsAccessAck, Implies, AccessAckAP(beatBytes))
  val AccessAckDataProperty = qProp[TLChannel, Int, UInt](IsAccessAckData, Implies, AccessAckDataAP(beatBytes))
  val HintAckProperty = qProp[TLChannel, Int, UInt](IsHintAck, Implies, HintAckAP(beatBytes))

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
  val PutPartiaFourBeatDataHandshakeProperty = qProp[TLChannel, Int, UInt](IsPutPartial & SaveSource & FourBeat(beatBytes), Implies, ###(1,-1), IsAccessAck & CheckSource,
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

  val allProperties = Seq(
    GetProperty,
    PutPullProperty,
    PutPartialProperty,
    ArithmeticProperty,
    LogicalProperty,
    HintProperty,
    AccessAckProperty,
    AccessAckDataProperty,
    HintAckProperty,
    GetDataSingleBeatHandshakeProperty,
    PutFullSingleBeatDataHandshakeProperty,
    PutPartialSingleBeatDataHandshakeProperty,
    ArithSingleBeatDataHandhsakeProperty,
    LogicSingleBeatDataHandhsakeProperty,
    GetDataTwoBeatHandshakeProperty,
    PutFullTwoBeatDataHandshakeProperty,
    PutPartialTwoBeatDataHandshakeProperty,
    ArithTwoBeatDataHandhsakeProperty,
    LogicTwoBeatDataHandhsakeProperty,
    GetDataFourBeatHandshakeProperty,
    PutPartiaFourBeatDataHandshakeProperty,
    ArithFourBeatDataHandhsakeProperty,
    LogicFourBeatDataHandhsakeProperty,
    HintHandhsakeProperty
  )
}

// NOTE: Currently only supports TL-U
class TLSLProtocolChecker(params: TLBundleParameters, sparam: TLSlaveParameters, mparam: TLMasterParameters) {
  
}
