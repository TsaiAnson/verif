package verif

import freechips.rocketchip.tilelink._
import verif.TLUtils._
import chisel3._
import chisel3.util.log2Ceil
import freechips.rocketchip.diplomacy.TransferSizes
import scala.collection.mutable
import scala.collection.immutable
import TLTransaction._

// Checks the protocol compliance of transactions exchanged on a *single* connection
class TLProtocolChecker(params: TLBundleParameters, sparam: TLSlaveParameters, mparam: TLMasterParameters) {

  // Internal state mapping source -> state
  // States: 0 (Idle), 1 (pending AccessAck), 2 (pending HintAck), 3 (pending Grant), 4 (pending GrantAck), 5 (pending ReleaseAck),
  //       : 1X (pending ProbeAck - X is previous state as Probes can interrupt existing transaction)
  val sourceState = new mutable.HashMap[Int,Int]()
  val sinkState = new mutable.HashMap[Int,Int]()

  // Internal state for burst parameter checking (source -> head of burst)
  val sourceBurstHead = new mutable.HashMap[Int,TLChannel]()
  // Internal state for burst counting (source -> -X where X is # of remaining beats)
  val sourceBurstRemain = new mutable.HashMap[Int,Int]()
  val beatSize = log2Ceil(params.dataBits / 8)

  // Protocol compliance checker
  def check(txns: Seq[TLChannel]) : Unit = {
    for (txn <- txns) {
      txn match {
        case txna: TLBundleA =>
          if (txna.opcode.litValue() == TLOpcodes.PutFullData) {

            // Assertions checking on first TLBundle
            assert(sparam.supportsPutFull != TransferSizes.none, "(A) Channel does not support PUTFULL requests.")
            assert(txna.param.litValue() == 0, "(A) Non-zero param field for PUTFULL TLBundle")
            assert(containsLg(sparam.supportsPutFull, txna.size), "(A) PUTFULL Size is outside of valid transfer sizes")
            assert(alignedLg(txna.address, txna.size), s"(A) PUTFULL Address (${txna.address}) is NOT aligned with size (${txna.size})")
            if (txna.size.litValue() < beatSize) {
              assert(alignedMaskLg(txna.mask, txna.size), s"(A) PUTFULL Mask (${txna.mask}) is not aligned with size (${txna.size})")
            } else {
              assert(alignedMaskLg(txna.mask, beatSize.U), s"(A) PUTFULL (Burst) Mask (${txna.mask}) is not aligned with beat size ($beatSize)")
            }
            assert(contiguous(txna.mask), "(A) PUTFULL MASK is not contiguous")
            assert(!txna.corrupt.litToBoolean, "(A) Corrupt PUTFULL TLBundle")

            // Burst helper function
            burstAHelper(txna)

          } else if (txna.opcode.litValue() == TLOpcodes.PutPartialData) {

            // Assertions checking on first TLBundle
            assert(sparam.supportsPutPartial != TransferSizes.none, "(A) Channel does not support PUTPARTIAL requests.")
            assert(txna.param.litValue() == 0, "(A) Non-zero param field for PUTPARTIAL TLBundle")
            assert(containsLg(sparam.supportsPutPartial, txna.size), "(A) PUTPARTIAL Size is outside of valid transfer sizes")
            assert(alignedLg(txna.address, txna.size), s"(A) PUTPARTIAL Address (${txna.address}) is NOT aligned with size (${txna.size})")
            // TODO Check that high bits are aligned
            assert(!txna.corrupt.litToBoolean, "(A) Corrupt PUTPARTIAL TLBundle")

            // Burst helper function
            burstAHelper(txna)

          } else if (txna.opcode.litValue() == TLOpcodes.ArithmeticData) {

            // Assertions checking on first TLBundle
            assert(sparam.supportsArithmetic != TransferSizes.none, "(A) Channel does not support ARITHMETIC requests.")
            assert(txna.param.litValue() >= 0 && txna.param.litValue() <= 4, s"(A) Non-valid PARAM (${txna.param}) for ARITHMETIC Data Bundle")
            assert(containsLg(sparam.supportsArithmetic, txna.size), "(A) ARITHMETIC Size is outside of valid transfer sizes")
            assert(alignedLg(txna.address, txna.size), s"(A) ARITHMETIC Address (${txna.address}) is NOT aligned with size (${txna.size})")
            if (txna.size.litValue() < beatSize) {
              assert(alignedMaskLg(txna.mask, txna.size), s"(A) ARITHMETIC Mask (${txna.mask}) is not aligned with size (${txna.size})")
            } else {
              assert(alignedMaskLg(txna.mask, beatSize.U), s"(A) ARITHMETIC (Burst) Mask (${txna.mask}) is not aligned with beat size ($beatSize)")
            }
            assert(contiguous(txna.mask), "(A) ARITHMETIC MASK is not contiguous")
            assert(!txna.corrupt.litToBoolean, "(A) Corrupt ARITHMETIC TLBundle")

            // Burst helper function
            burstAHelper(txna)

          } else if (txna.opcode.litValue() == TLOpcodes.LogicalData) {

            // Assertions checking on first TLBundle
            assert(sparam.supportsLogical != TransferSizes.none, "(A) Channel does not support LOGIC requests.")
            assert(txna.param.litValue() >= 0 && txna.param.litValue() <= 3, s"(A) Non-valid PARAM (${txna.param}) for LOGIC Data Bundle")
            assert(containsLg(sparam.supportsLogical, txna.size), "(A) LOGIC Size is outside of valid transfer sizes")
            assert(alignedLg(txna.address, txna.size), s"(A) LOGIC Address (${txna.address}) is NOT aligned with size (${txna.size})")
            if (txna.size.litValue() < beatSize) {
              assert(alignedMaskLg(txna.mask, txna.size), s"(A) LOGIC Mask (${txna.mask}) is not aligned with size (${txna.size})")
            } else {
              assert(alignedMaskLg(txna.mask, beatSize.U), s"(A) LOGIC (Burst) Mask (${txna.mask}) is not aligned with beat size ($beatSize)")
            }
            assert(contiguous(txna.mask), "(A) LOGICAL MASK is not contiguous")
            assert(!txna.corrupt.litToBoolean, "(A) Corrupt LOGICAL TLBundle")

            // Burst helper function
            burstAHelper(txna)

          } else if (txna.opcode.litValue() == TLOpcodes.Get) {

            // Assertions checking on first TLBundle
            assert(sparam.supportsGet != TransferSizes.none, "(A) Channel does not support GET requests.")
            assert(txna.param.litValue() == 0, "(A) Non-zero param field for GET TLBundle")
            assert(containsLg(sparam.supportsGet, txna.size), "(A) GET Size is outside of valid transfer sizes")
            if (txna.size.litValue() < beatSize) {
              assert(alignedMaskLg(txna.mask, txna.size), s"(A) GET Mask (${txna.mask}) is not aligned with size (${txna.size})")
            } else {
              assert(alignedMaskLg(txna.mask, beatSize.U), s"(A) GET (Burst) Mask (${txna.mask}) is not aligned with beat size ($beatSize)")
            }
            assert(contiguous(txna.mask), "(A) GET MASK is not contiguous")
            assert(!txna.corrupt.litToBoolean, "(A) Corrupt GET TLBundle")

            // Update state
            setResponseState(txna, encodeChannel(txna.source.litValue().toInt, 'A'))

          } else if (txna.opcode.litValue() == TLOpcodes.Hint) {

            assert(sparam.supportsHint != TransferSizes.none, "(A) Channel does not support INTENT requests.")
            assert(txna.param.litValue() >= 0 && txna.param.litValue() <= 1, s"(A) Non-valid PARAM (${txna.param}) for INTENT Data Bundle")
            assert(containsLg(sparam.supportsHint, txna.size), "(A) INTENT Size is outside of valid transfer sizes")
            if (txna.size.litValue() < beatSize) {
              assert(alignedMaskLg(txna.mask, txna.size), s"(A) INTENT Mask (${txna.mask}) is not aligned with size (${txna.size})")
            } else {
              assert(alignedMaskLg(txna.mask, beatSize.U), s"(A) INTENT (Burst) Mask (${txna.mask}) is not aligned with beat size ($beatSize)")
            }
            assert(!txna.corrupt.litToBoolean, "(A) Corrupt INTENT TLBundle")

            // Update state
            setResponseState(txna, encodeChannel(txna.source.litValue().toInt, 'A'))

          } else if (txna.opcode.litValue() == TLOpcodes.AcquireBlock) {

            assert(sparam.supportsAcquireB != TransferSizes.none, "(A) Channel does not support AcquireB requests.")
            assert(sparam.supportsAcquireT != TransferSizes.none, "(A) Channel does not support AcquireT requests.")
            assert(txna.param.litValue() >= 0 && txna.param.litValue() <= 3, s"(A) Non-valid PARAM (${txna.param}) for ACQUIREBLOCK Bundle")
            assert(containsLg(sparam.supportsAcquireT, txna.size), "(A) ACQUIRE Size is outside of valid transfer sizes")
            if (txna.size.litValue() < beatSize) {
              assert(alignedMaskLg(txna.mask, txna.size), s"(A) ACQUIREBLOCK Mask (${txna.mask}) is not aligned with size (${txna.size})")
            } else {
              assert(alignedMaskLg(txna.mask, beatSize.U), s"(A) ACQUIREBLOCK (Burst) Mask (${txna.mask}) is not aligned with beat size ($beatSize)")
            }
            assert(contiguous(txna.mask), "(A) ACQUIREBLOCK MASK is not contiguous")
            assert(!txna.corrupt.litToBoolean, "(A) Corrupt ACQUIREBLOCK TLBundle")

            // Update state
            setResponseState(txna, encodeChannel(txna.source.litValue().toInt, 'A'))

          } else if (txna.opcode.litValue() == TLOpcodes.AcquirePerm) {

            assert(sparam.supportsAcquireB != TransferSizes.none, "(A) Channel does not support AcquireB requests.")
            assert(sparam.supportsAcquireT != TransferSizes.none, "(A) Channel does not support AcquireT requests.")
            assert(txna.param.litValue() >= 0 && txna.param.litValue() <= 3, s"(A) Non-valid PARAM (${txna.param}) for ACQUIREPERM Bundle")
            assert(containsLg(sparam.supportsAcquireT, txna.size), "(A) ACQUIRE Size is outside of valid transfer sizes")
            if (txna.size.litValue() < beatSize) {
              assert(alignedMaskLg(txna.mask, txna.size), s"(A) ACQUIREPERM Mask (${txna.mask}) is not aligned with size (${txna.size})")
            } else {
              assert(alignedMaskLg(txna.mask, beatSize.U), s"(A) ACQUIREPERM (Burst) Mask (${txna.mask}) is not aligned with beat size ($beatSize)")
            }
            assert(contiguous(txna.mask), "(A) LOGICAL MASK is not contiguous")
            assert(!txna.corrupt.litToBoolean, "(A) Corrupt ACQUIREPERM TLBundle")

            // Update state
            setResponseState(txna, encodeChannel(txna.source.litValue().toInt, 'A'))

          } else {
            assert(false, "(A) Invalid OPCODE on A Channel")
          }

        case txnb: TLBundleB =>
          // NOTE: Ignoring checking TL-UL/UH messages forwarded on B channel
          if (txnb.opcode.litValue() == TLOpcodes.ProbeBlock) {

            // Assertions checking on first TLBundle
            assert(mparam.supports.probe != TransferSizes.none, "(B) Channel does not support PROBEBLOCK requests.")
            assert(txnb.param.litValue() >= 0 && txnb.param.litValue() < 3, s"(B) Non-valid PARAM (${txnb.param}) for PROBEBLOCK Bundle")
            if (txnb.size.litValue() < beatSize) {
              assert(alignedMaskLg(txnb.mask, txnb.size), s"(B) PROBEBLOCK Mask (${txnb.mask}) is not aligned with size (${txnb.size})")
            } else {
              assert(alignedMaskLg(txnb.mask, beatSize.U), s"(B) PROBEBLOCK (Burst) Mask (${txnb.mask}) is not aligned with beat size ($beatSize)")
            }
            assert(contiguous(txnb.mask), "(B) PROBEBLOCK MASK is not contiguous")
            assert(!txnb.corrupt.litToBoolean, "(B) Corrupt PROBEBLOCK TLBundle")

            // Update state
            setResponseState(txnb, encodeChannel(txnb.source.litValue().toInt, 'B'))

          } else if (txnb.opcode.litValue() == TLOpcodes.ProbePerm) {

            // Assertions checking on first TLBundle
            assert(mparam.supports.probe != TransferSizes.none, "(B) Channel does not support PROBEPERM requests.")
            assert(txnb.param.litValue() >= 0 && txnb.param.litValue() < 3, s"(B) Non-valid PARAM (${txnb.param}) for PROBEPERM Bundle")
            if (txnb.size.litValue() < beatSize) {
              assert(alignedMaskLg(txnb.mask, txnb.size), s"(B) PROBEPERM Mask (${txnb.mask}) is not aligned with size (${txnb.size})")
            } else {
              assert(alignedMaskLg(txnb.mask, beatSize.U), s"(B) PROBEPERM (Burst) Mask (${txnb.mask}) is not aligned with beat size ($beatSize)")
            }
            assert(contiguous(txnb.mask), "(B) PROBEPERM MASK is not contiguous")
            assert(!txnb.corrupt.litToBoolean, "(B) Corrupt PROBEPERM TLBundle")

            // Update state
            setResponseState(txnb, encodeChannel(txnb.source.litValue().toInt, 'B'))

          } else {
            assert(false, "(B) Invalid OPCODE on B Channel")
          }

        case txnc: TLBundleC =>
          // NOTE: Ignoring checking TL-UL/UH messages forwarded on C channel
          if (txnc.opcode.litValue() == TLOpcodes.ProbeAck) {

            // Assertions checking on first TLBundle
            assert(mparam.supports.probe != TransferSizes.none, "(C) Channel does not support PROBEACK requests.")
            assert(txnc.param.litValue() >= 0 && txnc.param.litValue() < 6, s"(C) Non-valid PARAM (${txnc.param}) for PROBEACK Bundle")
            assert(alignedLg(txnc.address, txnc.size), s"(C) PROBEACK Address (${txnc.address}) is NOT aligned with size (${txnc.size})")

            // Update state
            setResponseState(txnc, encodeChannel(txnc.source.litValue().toInt, 'C'))

          } else if (txnc.opcode.litValue() == TLOpcodes.ProbeAckData) {

            // Assertions checking on first TLBundle
            assert(mparam.supports.probe != TransferSizes.none, "(C) Channel does not support PROBEACKDATA requests.")
            assert(txnc.param.litValue() >= 0 && txnc.param.litValue() < 6, s"(C) Non-valid PARAM (${txnc.param}) for PROBEACKDATA Bundle")
            assert(alignedLg(txnc.address, txnc.size), s"(C) PROBEACKDATA Address (${txnc.address}) is NOT aligned with size (${txnc.size})")

            // Burst helper function
            burstCHelper(txnc)

          } else if (txnc.opcode.litValue() == TLOpcodes.Release) {

            assert(sparam.supportsAcquireB != TransferSizes.none, "(C) Channel does not support AcquireB, and thus RELEASE, requests.")
            assert(txnc.param.litValue() >= 0 && txnc.param.litValue() < 6, s"(B) Non-valid PARAM (${txnc.param}) for RELEASE Bundle")
            assert(!txnc.corrupt.litToBoolean, "(C) Corrupt RELEASE TLBundle")

            // Update state
            setResponseState(txnc, encodeChannel(txnc.source.litValue().toInt, 'C'))

          } else if (txnc.opcode.litValue() == TLOpcodes.ReleaseData) {

            // Assertions checking on first TLBundle
            assert(sparam.supportsAcquireB != TransferSizes.none, "(C) Channel does not support AcquireB, and thus RELEASE, requests.")
            assert(txnc.param.litValue() >= 0 && txnc.param.litValue() < 6, s"(C) Non-valid PARAM (${txnc.param}) for RELEASEDATA Bundle")
            assert(alignedLg(txnc.address, txnc.size), s"(C) RELEASEDATA Address (${txnc.address}) is NOT aligned with size (${txnc.size})")

            // Burst helper function
            burstCHelper(txnc)

          } else {
            assert(false, "(C) Invalid OPCODE on C Channel")
          }

        case txnd: TLBundleD =>
          if (txnd.opcode.litValue() == TLOpcodes.AccessAck) {

            assert(txnd.param.litValue() == 0, "(D) Non-zero param field for ACCESSACK TLBundle")
            assert(!txnd.corrupt.litToBoolean, "(D) Non-zero corrupt field for ACCESSACK TLBundle")

            // Update state
            setResponseState(txnd, encodeChannel(txnd.source.litValue().toInt, 'D'))

          } else if (txnd.opcode.litValue() == TLOpcodes.AccessAckData) {

            // Assertions checking on first TLBundle
            assert(txnd.param.litValue() == 0, "(D) Non-zero param field for ACCESSACKDATA TLBundle")
            if (txnd.denied.litToBoolean) {
              assert(txnd.corrupt.litToBoolean, "(D) ACCESSACKDATA denied but not corrupt")
            }

            // Burst helper function
            burstDHelper(txnd)

          } else if (txnd.opcode.litValue() == TLOpcodes.HintAck) {

            assert(txnd.param.litValue() == 0, "(D) Non-zero param field for HINTACK TLBundle")
            assert(!txnd.corrupt.litToBoolean, "(D) Non-zero corrupt field for HINTACK TLBundle")

            // Update state
            setResponseState(txnd, encodeChannel(txnd.source.litValue().toInt, 'D'))

          } else if (txnd.opcode.litValue() == TLOpcodes.Grant) {

            assert(txnd.param.litValue() >= 0 && txnd.param.litValue() < 3, "(D) Non-valid param field for GRANT TLBundle")
            assert(!txnd.corrupt.litToBoolean, "(D) Non-zero corrupt field for GRANT TLBundle")

            // Update state
            setResponseState(txnd, encodeChannel(txnd.source.litValue().toInt, 'D'))

          } else if (txnd.opcode.litValue() == TLOpcodes.GrantData) {

            // Assertions checking on first TLBundle
            assert(txnd.param.litValue() >= 0 && txnd.param.litValue() < 3, "(D) Non-valid param field for GRANTDATA TLBundle")
            if (txnd.denied.litToBoolean) {
              assert(txnd.corrupt.litToBoolean, "(D) GRANTDATA denied but not corrupt")
            }

            // Burst helper function
            burstDHelper(txnd)

          } else if (txnd.opcode.litValue() == TLOpcodes.ReleaseAck) {

            assert(txnd.param.litValue() == 0, "(D) Non-zero param field for RELEASEACK TLBundle")
            assert(!txnd.corrupt.litToBoolean, "(D) Corrupt RELEASEACK TLBundle")
            assert(!txnd.denied.litToBoolean, "(D) RELEASEACK cannot be denied.")

            // Update state
            setResponseState(txnd, encodeChannel(txnd.source.litValue().toInt, 'D'))

          } else {
            assert(false, "(D) Invalid OPCODE on D Channel")
          }
        case txne: TLBundleE =>
          // No asserts to check

          // Update state
          setResponseState(txne, encodeChannel(txne.sink.litValue().toInt, 'E'))
      }
    }
  }

  // Helper Function to Encode Channel and Source (Since source IDs are only unique to a *single* channel)
  // 1: A, 2: B, 2: C, 1: D, 1: E
  def encodeChannel(source: Int, channel: Char): Int = {
    val channelMap = immutable.HashMap[Char, Int]('A' -> 1, 'B' -> 2, 'C' -> 2, 'D' -> 1, 'E' -> 1)

    if (!channelMap.contains(channel)) println(s"WARNING: Given channel ($channel) is not valid (A, B, C, D, or E)")
    (source * 10) + channelMap.getOrElse(channel, 0)
  }

  // Helper Functions for Burst Checking
  def burstAHelper(txna: TLBundleA): Unit = {
    // If bundles are in a burst
    val sourceKey = encodeChannel(txna.source.litValue().toInt, 'A')
    if (!isNonBurst(txna) && txna.size.litValue() > beatSize) {
      if (sourceBurstRemain.getOrElse(sourceKey,0) == 0) {
        // Start of burst
        sourceBurstHead(sourceKey) = txna
        sourceBurstRemain(sourceKey) = 1 - (1 << (txna.size.litValue().toInt - beatSize))
      } else {
        // Checking constant parameters with head of burst
        val head = sourceBurstHead(sourceKey)
        head match {
          case heada: TLBundleA =>
            var result = true
            result &= txna.param.litValue() == heada.param.litValue()
            result &= txna.size.litValue() == heada.size.litValue()
            result &= txna.source.litValue() == heada.source.litValue()
            result &= txna.address.litValue() == heada.address.litValue()
            assert(result, s"(A) A constant field was modified within a beat. Burst head: ${sourceBurstHead(sourceKey)}. Given beat: $txna")

            sourceBurstRemain(sourceKey) += 1
            if (sourceBurstRemain(sourceKey) == 0) setResponseState(sourceBurstHead(sourceKey), sourceKey)
          case _ =>
            assert(false, s"Expected remaining ${0 - sourceBurstRemain(sourceKey)} beats of burst. Burst head: ${sourceBurstHead(sourceKey)}. Given beat: $txna")
        }
      }
    } else if (sourceBurstRemain.getOrElse(sourceKey,0) < 0) {
      assert(false, s"Expected remaining ${0 - sourceBurstRemain(sourceKey)} beats of burst. Burst head: ${sourceBurstHead(sourceKey)}. Given beat: $txna")
    } else {
      setResponseState(txna, sourceKey)
    }
  }

  def burstCHelper(txnc: TLBundleC): Unit = {
    // If bundles are in a burst
    val sourceKey = encodeChannel(txnc.source.litValue().toInt, 'C')
    if (!isNonBurst(txnc) && txnc.size.litValue() > beatSize) {
      if (sourceBurstRemain.getOrElse(sourceKey,0) == 0) {
        // Start of burst
        sourceBurstHead(sourceKey) = txnc
        sourceBurstRemain(sourceKey) = 1 - (1 << (txnc.size.litValue().toInt - beatSize))
      } else {
        // Checking constant parameters with head of burst
        val head = sourceBurstHead(sourceKey)
        head match {
          case headc: TLBundleC =>
            var result = true
            result &= txnc.param.litValue() == headc.param.litValue()
            result &= txnc.size.litValue() == headc.size.litValue()
            result &= txnc.source.litValue() == headc.source.litValue()
            result &= txnc.address.litValue() == headc.address.litValue()
            assert(result, s"(C) A constant field was modified within a beat. Burst head: ${sourceBurstHead(sourceKey)}. Given beat: $txnc")

            sourceBurstRemain(sourceKey) += 1
            if (sourceBurstRemain(sourceKey) == 0) setResponseState(sourceBurstHead(sourceKey), sourceKey)
          case _ =>
            assert(false, s"Expected remaining ${0 - sourceBurstRemain(sourceKey)} beats of burst. Burst head: ${sourceBurstHead(sourceKey)}. Given beat: $txnc")
        }
      }
    } else if (sourceBurstRemain.getOrElse(sourceKey,0) < 0) {
      assert(false, s"Expected remaining ${0 - sourceBurstRemain(sourceKey)} beats of burst. Burst head: ${sourceBurstHead(sourceKey)}. Given beat: $txnc")
    } else {
      setResponseState(txnc, sourceKey)
    }
  }

  def burstDHelper(txnd: TLBundleD): Unit = {
    // If bundles are in a burst
    val sourceKey = encodeChannel(txnd.source.litValue().toInt, 'D')
    if (!isNonBurst(txnd) && txnd.size.litValue() > beatSize) {
      if (sourceBurstRemain.getOrElse(sourceKey,0) == 0) {
        // Start of burst
        sourceBurstHead(sourceKey) = txnd
        sourceBurstRemain(sourceKey) = 1 - (1 << (txnd.size.litValue().toInt - beatSize))
      } else {
        // Checking constant parameters with head of burst
        val head = sourceBurstHead(sourceKey)
        head match {
          case headd: TLBundleD =>
            var result = true
            result &= txnd.param.litValue() == headd.param.litValue()
            result &= txnd.size.litValue() == headd.size.litValue()
            result &= txnd.source.litValue() == headd.source.litValue()
            result &= txnd.sink.litValue() == headd.sink.litValue()
            result &= txnd.denied.litValue() == headd.denied.litValue()
            assert(result, s"(D) A constant field was modified within a beat. Burst head: ${sourceBurstHead(sourceKey)}. Given beat: $txnd")

            sourceBurstRemain(sourceKey) += 1
            if (sourceBurstRemain(sourceKey) == 0) setResponseState(sourceBurstHead(sourceKey), sourceKey)
          case _ =>
            assert(false, s"Expected remaining ${0 - sourceBurstRemain(sourceKey)} beats of burst. Burst head: ${sourceBurstHead(sourceKey)}. Given beat: $txnd")
        }
      }
    } else if (sourceBurstRemain.getOrElse(sourceKey,0) < 0) {
      assert(false, s"Expected remaining ${0 - sourceBurstRemain(sourceKey)} beats of burst. Burst head: ${sourceBurstHead(sourceKey)}. Given beat: $txnd")
    } else {
      setResponseState(txnd, sourceKey)
    }
  }

  // Helper Function to check and set response state (non-burst)
  def setResponseState(txn: TLChannel, sourceKey: Int): Unit = {
    assert(sourceState.getOrElse(sourceKey, 0) >= 0)
    txn match {
      case txna: TLBundleA =>
        assert(sourceState.getOrElse(sourceKey, 0) == 0, s"ERROR: Either concurrent transactions with same source ID or missed Ack from slave.")
        val opcode = txna.opcode.litValue()
        sourceState(sourceKey) = if (opcode <= 4) 1 else if (opcode == 5) 2 else 3
      case _: TLBundleB =>
        assert(sourceState.getOrElse(sourceKey, 0) < 10, s"ERROR: Concurrent Probes with the same source ID.")
        sourceState(sourceKey) = sourceState.getOrElse(sourceKey, 0) + 10
      case txnc: TLBundleC =>
        if (txnc.opcode.litValue().toInt == TLOpcodes.ProbeAck || txnc.opcode.litValue().toInt == TLOpcodes.ProbeAckData) {
          assert(sourceState.getOrElse(sourceKey, 0) >= 10, s"ERROR: Unexpected ProbeAck/Data (no pending Probe with same sourceID)")
          sourceState(sourceKey) = sourceState(sourceKey) % 10
        } else if (txnc.opcode.litValue().toInt == TLOpcodes.Release || txnc.opcode.litValue().toInt == TLOpcodes.ReleaseData) {
          assert(sourceState.getOrElse(sourceKey, 0) == 0, s"ERROR: Unexpected release operation (another operation with same sourceID is still in progress)")
          sourceState(sourceKey) = 5
        }
      case txnd: TLBundleD =>
        if (txnd.opcode.litValue().toInt == TLOpcodes.AccessAck || txnd.opcode.litValue().toInt == TLOpcodes.AccessAckData) {
          assert(sourceState.getOrElse(sourceKey, 0) == 1, s"ERROR: Unexpected AccessAck/Data operation (no pending operation with same sourceID)")
          sourceState(sourceKey) = 0
        } else if (txnd.opcode.litValue().toInt == TLOpcodes.HintAck) {
          assert(sourceState.getOrElse(sourceKey, 0) == 2, s"ERROR: Unexpected HintAck operation (no Hint with same sourceID in progress)")
          sourceState(sourceKey) = 0
        } else if (txnd.opcode.litValue().toInt == TLOpcodes.Grant || txnd.opcode.litValue().toInt == TLOpcodes.GrantData) {
          assert(sourceState.getOrElse(sourceKey, 0) == 3, s"ERROR: Unexpected Grant operation (no Acquire with same sourceID in progress)")
          sourceState(sourceKey) = 0
          sinkState(encodeChannel(txnd.sink.litValue().toInt, 'D')) = 4
        } else if (txnd.opcode.litValue().toInt == TLOpcodes.ReleaseAck) {
          // Special case: Since Release to ReleaseAck is from channel C -> channel D, we need to encode with C
          val newSourceKey = encodeChannel(sourceKey/10, 'C')
          assert(sourceState.getOrElse(newSourceKey, 0) == 5, s"ERROR: Unexpected ReleaseAck operation (no Release in progress with same sourceID)")
          sourceState(newSourceKey) = 0
        }
      case tnxe: TLBundleE =>
        val sinkKey = encodeChannel(tnxe.sink.litValue().toInt, 'D')
        assert(sinkState.getOrElse(sinkKey, 0) == 4, s"ERROR: Unexpected GrantAck operation (no Acquire in progress or Grant received with same sourceID)")
        sinkState(sinkKey) = 0
    }
  }
}