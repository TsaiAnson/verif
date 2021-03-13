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
    t match {case t: TLBundleA => t.opcode.litValue() == TLOpcodes.Get; case _: TLBundleD => false}})
  val IsPutFullOp = qAP({ (t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.opcode.litValue() == TLOpcodes.PutFullData; case _: TLBundleD => false}})
  val IsPutPartialOp = qAP({ (t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.opcode.litValue() == TLOpcodes.PutPartialData; case _: TLBundleD => false}})
  val IsArithOp = qAP({ (t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.opcode.litValue() == TLOpcodes.ArithmeticData; case _: TLBundleD => false}})
  val IsLogicOp = qAP({ (t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.opcode.litValue() == TLOpcodes.LogicalData; case _: TLBundleD => false}})
  val IsHintOp = qAP({ (t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.opcode.litValue() == TLOpcodes.Hint; case _: TLBundleD => false}})

  // Channel D
  val IsAccessAckOp = qAP({ (t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case _: TLBundleA => false; case t: TLBundleD => t.opcode.litValue() == TLOpcodes.AccessAck}})
  val IsAccessAckDataOp = qAP({ (t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case _: TLBundleA => false; case t: TLBundleD => t.opcode.litValue() == TLOpcodes.AccessAckData}})
  val IsHintAckOp = qAP({ (t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case _: TLBundleA => false; case t: TLBundleD => t.opcode.litValue() == TLOpcodes.HintAck}})
}

trait TLStaticParameterAP {
  val ZeroParam = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.param.litValue() == 0; case t: TLBundleD => t.param.litValue() == 0}})
  val ArithParam = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.param.litValue() >= 0 && t.param.litValue() <= 4; case _: TLBundleD => false}})
  val LogicParam = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.param.litValue() >= 0 && t.param.litValue() <= 3; case _: TLBundleD => false}})
  val HintParam = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.param.litValue() >= 0 && t.param.litValue() <= 1; case _: TLBundleD => false}})
  val AlignedAddr = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => (t.address.litValue() & ((1 << t.size.litValue().toInt) - 1)) == 0; case _: TLBundleD => false}})
  val ContiguousMask = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => (t.mask.litValue() & (t.mask.litValue() + 1)) == 0; case _: TLBundleD => false}})
  val ZeroCorrupt = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.corrupt.litValue() == 0; case t: TLBundleD => t.corrupt.litValue() == 0}})
  val DeniedCorrupt = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case _: TLBundleA => false; case t: TLBundleD => if (t.denied.litToBoolean) {t.corrupt.litToBoolean} else {true}}})
}

// Requires parameters
trait TLDynamicParameterAP {
  def SizeWithinMaxTx(maxTransfer: Int) = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => t.size.litValue() >= 0 && t.size.litValue() <= log2Ceil(maxTransfer); case t: TLBundleD =>
      t.size.litValue() >= 0 && t.size.litValue() <= log2Ceil(maxTransfer)}})
  def MaskAllHigh(beatBytes: Int) = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => if (t.size.litValue() > log2Ceil(beatBytes)) {(1 << beatBytes) - 1 == t.mask.litValue()}
    else {(1 << (1 << t.size.litValue().toInt)) - 1 == t.mask.litValue()}; case _ => false}})
  def MaskWithinSize(beatBytes: Int) = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => if (t.size.litValue() > log2Ceil(beatBytes)) {(1 << beatBytes) > t.mask.litValue()}
                                  else {(1 << (1 << t.size.litValue().toInt)) > t.mask.litValue()}; case _ => false}})
}

trait TLModelingAPs {
  val SaveSource = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => {h("source") = t.source.litValue().toInt; true}; case _: TLBundleD => false}})
  val CheckSource = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case _: TLBundleA => false; case t: TLBundleD => h("source") == t.source.litValue()}})
  val SaveSize = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case t: TLBundleA => {h("size") = t.size.litValue().toInt; true}; case _: TLBundleD => false}})
  val CheckSize = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case _: TLBundleA => false; case t: TLBundleD => h("size") == t.size.litValue()}})
  val CheckData = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {case _: TLBundleA => false; case t: TLBundleD => if (m.isDefined) m.get.get(0).litValue() == t.data.litValue() else true}})
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

trait BurstSizeAP {
  def SizeCheck(beatCount: Int, beatBytes: Int) = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    // beatCount must be power of 2
    assert ((beatCount & (beatCount - 1)) == 0, s"beatCount must be a power of 2. Given: $beatCount")
    t match {
      case t: TLBundleA =>
        if (beatCount == 1) t.size.litValue().toInt <= log2Ceil(beatBytes)
        else t.size.litValue().toInt == log2Ceil(beatBytes) + log2Ceil(beatCount)
      case _: TLBundleD => false
    }}, s"AD: If Size is $beatCount Beats")
}

// Other Utility APs
trait MiscAP {
  // Increases/decreases counter for expected response transactions
  def CheckReqResp(beatBytes: Int, check: Boolean = false) = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
    t match {
      case t: TLBundleA =>
        if (t.opcode.litValue() == TLOpcodes.PutFullData || t.opcode.litValue() == TLOpcodes.PutPartialData) {
          // For burst requests (PutFull, PutPartial)
          if (h.getOrElse("burst_req", 0) != 0) h("burst_req") -= 1
          else {
            // If burst request, increment burst_req by number of remaining burst message (increments by 0 for non-burst reqs)
            h("burst_req") = h.getOrElse("burst_req", 0) + (1 << scala.math.max(t.size.litValue().toInt - log2Ceil(beatBytes), 0)) - 1
            h("expected_resp") = h.getOrElse("expected_resp", 0) + 1
          }
        } else if (t.opcode.litValue() == TLOpcodes.ArithmeticData || t.opcode.litValue() == TLOpcodes.LogicalData || t.opcode.litValue() == TLOpcodes.Hint) {
          // For Atomic (Arith and Logic) Instructions + Hint, where there is exactly one response per request bundle
          h("expected_resp") = h.getOrElse("expected_resp", 0) + 1
        } else {
          // For Get: Response transactions scaled by request size
          h("expected_resp") = h.getOrElse("expected_resp", 0) + (1 << scala.math.max(t.size.litValue().toInt - log2Ceil(beatBytes), 0))
        }
      case _: TLBundleD => h("expected_resp") = h.getOrElse("expected_resp", 0) - 1 // Using .getOrElse as the first transaction could be a response (bad trace)
    }
    if (check) {
      if (h("expected_resp") != 0) println(s"ERROR: Expected Responses Remaining: ${h("expected_resp")}")
      if (h("burst_req") != 0) println(s"ERROR: Expected Burst Requests Remaining: ${h("burst_req")}")
      h("expected_resp") == 0 && h("burst_req") == 0
    } else {
      true
    }
  }, "AD: CheckReqResp")
}

// maxDelay for message handshake checking (request to response cycle delay)
class TLUProperties(beatBytes: Int, maxDelay: Int) extends TLMessageAPs with TLModelingAPs  with BurstSizeAP with MiscAP {
  // Message Properties
  def GetProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt](IsGetOp + Implies + GetAP(beatBytes, maxTxSize))
  def PutPullProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt](IsPutFullOp + Implies + PutFullAP(beatBytes, maxTxSize))
  def PutPartialProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt](IsPutPartialOp + Implies + PutPartialAP(beatBytes, maxTxSize))
  def ArithProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt](IsArithOp + Implies + ArithAP(beatBytes, maxTxSize))
  def LogicProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt](IsLogicOp + Implies + LogicAP(beatBytes, maxTxSize))
  def HintProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt](IsHintOp + Implies + HintAP(beatBytes, maxTxSize))
  def AccessAckProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt](IsAccessAckOp + Implies + AccessAckAP(maxTxSize))
  def AccessAckDataProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt](IsAccessAckDataOp + Implies + AccessAckDataAP(maxTxSize))
  def HintAckProperty(maxTxSize: Int) = qProp[TLChannel, Int, UInt](IsHintAckOp + Implies + HintAckAP(maxTxSize))

  // Handshake Properties (Message properties not checked here, see above)
  // Defining helper sequences
  def GetInitSequence(beatCount: Int) = qSeq[TLChannel, Int, UInt](IsGetOp & SaveSource & SaveSize & SizeCheck(beatCount, beatBytes))
  def PutFullInitSequence(beatCount: Int) = qSeq[TLChannel, Int, UInt](IsPutFullOp & SaveSource & SaveSize & SizeCheck(beatCount, beatBytes))
  def PutPartialInitSequence(beatCount: Int) = qSeq[TLChannel, Int, UInt](IsPutPartialOp & SaveSource & SaveSize & SizeCheck(beatCount, beatBytes))
  def ArithInitSequence(beatCount: Int) = qSeq[TLChannel, Int, UInt](IsArithOp & SaveSource & SaveSize & SizeCheck(beatCount, beatBytes))
  def LogicInitSequence(beatCount: Int) = qSeq[TLChannel, Int, UInt](IsLogicOp & SaveSource & SaveSize & SizeCheck(beatCount, beatBytes))

  val AccessAckCheckSequence = qSeq[TLChannel, Int, UInt](###(1, maxDelay), IsAccessAckOp & CheckSource & CheckSize)
  val AccessAckDataCheckSequence = qSeq[TLChannel, Int, UInt](###(1, maxDelay), IsAccessAckDataOp & CheckSource & CheckSize & CheckData)

  // Handshake Properties (supports burst of any size)
  def GetDataHandshakeProperties(maxTxSize: Int): Seq[Property[TLChannel, Int, UInt]] = {
    assert(maxTxSize % beatBytes == 0)
    val maxBeats = maxTxSize/beatBytes
    var beatCount = 1
    var result = Seq[Property[TLChannel, Int, UInt]]()
    while (beatCount <= maxBeats) {
      result = result :+ qProp[TLChannel, Int, UInt](GetInitSequence(beatCount) + Implies + (AccessAckDataCheckSequence * beatCount),
        s"GetDataHandshake: $beatCount Beats")
      beatCount= beatCount << 1
    }
    result
  }
  def PutFullHandshakeProperties(maxTxSize: Int): Seq[Property[TLChannel, Int, UInt]] = {
    assert(maxTxSize % beatBytes == 0)
    val maxBeats = maxTxSize/beatBytes
    var beatCount = 1
    var result = Seq[Property[TLChannel, Int, UInt]]()
    while (beatCount <= maxBeats) {
      result = result :+ qProp[TLChannel, Int, UInt]((PutFullInitSequence(beatCount) + Implies + ###(1, maxDelay)) * beatCount + AccessAckCheckSequence,
        s"PutFullHandshake: $beatCount Beats")
      beatCount= beatCount << 1
    }
    result
  }
  def PutPartialHandshakeProperties(maxTxSize: Int): Seq[Property[TLChannel, Int, UInt]] = {
    assert(maxTxSize % beatBytes == 0)
    val maxBeats = maxTxSize/beatBytes
    var beatCount = 1
    var result = Seq[Property[TLChannel, Int, UInt]]()
    while (beatCount <= maxBeats) {
      result = result :+ qProp[TLChannel, Int, UInt]((PutPartialInitSequence(beatCount) + Implies + ###(1, maxDelay)) * beatCount + AccessAckCheckSequence,
        s"PutFullHandshake: $beatCount Beats")
      beatCount= beatCount << 1
    }
    result
  }
  def ArithHandshakeProperties(maxTxSize: Int): Seq[Property[TLChannel, Int, UInt]] = {
    assert(maxTxSize % beatBytes == 0)
    val maxBeats = maxTxSize/beatBytes
    var beatCount = 1
    var result = Seq[Property[TLChannel, Int, UInt]]()
    while (beatCount <= maxBeats) {
      result = result :+ qProp[TLChannel, Int, UInt]((ArithInitSequence(beatCount) + Implies + ###(1, maxDelay)) * beatCount + (AccessAckDataCheckSequence * beatCount),
        s"PutFullHandshake: $beatCount Beats")
      beatCount= beatCount << 1
    }
    result
  }
  def LogicHandshakeProperties(maxTxSize: Int): Seq[Property[TLChannel, Int, UInt]] = {
    assert(maxTxSize % beatBytes == 0)
    val maxBeats = maxTxSize/beatBytes
    var beatCount = 1
    var result = Seq[Property[TLChannel, Int, UInt]]()
    while (beatCount <= maxBeats) {
      result = result :+ qProp[TLChannel, Int, UInt]((LogicInitSequence(beatCount) + Implies + ###(1, maxDelay)) * beatCount + (AccessAckDataCheckSequence * beatCount),
        s"PutFullHandshake: $beatCount Beats")
      beatCount= beatCount << 1
    }
    result
  }

  val HintHandshakeProperty = qProp[TLChannel, Int, UInt]((IsHintOp & SaveSize & SaveSource) + Implies + ###(1,-1) + (IsHintAckOp & CheckSize & CheckSource))

  // Checks if Request count matches Response count
  def CheckReqRespProperty(traceSize: Int) = qProp[TLChannel, Int, UInt](((CheckReqResp(beatBytes) + Implies) + ###(1,1)) + ((CheckReqResp(beatBytes) + ###(1,1)) * (traceSize - 2)) + CheckReqResp(beatBytes, true))

  // Helper methods to get properties
  def GetProperties(maxTxSize: Int): Seq[Property[TLChannel, Int, UInt]] = {
    Seq(GetProperty(maxTxSize)) ++ GetDataHandshakeProperties(maxTxSize)
  }

  def PutFullProperties(maxTxSize: Int): Seq[Property[TLChannel, Int, UInt]] = {
    Seq(PutPullProperty(maxTxSize)) ++ PutFullHandshakeProperties(maxTxSize)
  }

  def PutPartialProperties(maxTxSize: Int): Seq[Property[TLChannel, Int, UInt]] = {
    Seq(PutPartialProperty(maxTxSize)) ++ PutPartialHandshakeProperties(maxTxSize)
  }

  def ArithProperties(maxTxSize: Int): Seq[Property[TLChannel, Int, UInt]] = {
    Seq(ArithProperty(maxTxSize)) ++ ArithHandshakeProperties(maxTxSize)
  }

  def LogicProperties(maxTxSize: Int): Seq[Property[TLChannel, Int, UInt]] = {
    Seq(LogicProperty(maxTxSize)) ++ LogicHandshakeProperties(maxTxSize)
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
  // PB for Property Bank. maxDelay scales off of # of available source IDs, may need to change later
  val pb = new TLUProperties(sparam.beatBytes, (1 << bparam.sourceBits) * 3)
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
    var totalTxnCoverage = Array.fill(txns.size)(0) //
    for (property <- checkedProperties) {
      val temp = property.check(txns, memoryStates)
      if (!temp) println(s"Property failed: $property")
      result &= temp
      totalTxnCoverage = totalTxnCoverage.zip(property.txnCoverage).map{ case (x,y) => x + y}
    }

    // Checking Request to Response Count
    result &= pb.CheckReqRespProperty(txns.size).check(txns)

    // Checking for any overlapping coverage coverage (2 is expected: 1 for message field checking and 1 for message handshake)
    val overlappingCoverage = totalTxnCoverage.zipWithIndex.filter(_._1 > 2).map{ case (x,y) => (y, x-1)}
    if (overlappingCoverage.nonEmpty) {
      result &= false
      println(s"ERROR: The following transactions were matched multiple times across message handshake properties: (INDEX, MATCH COUNT)")
      println(overlappingCoverage.mkString("[", ", ", "]"))
      println(s"This means that there is an unmatched request or unexpected response.")
    }

    if (!result) println(s"One or more properties failed. Please check the above log.")
    result
  }
}
