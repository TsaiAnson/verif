package verif

import freechips.rocketchip.tilelink._
import verif.TLUtils._
import chisel3._
import chisel3.util.log2Ceil
import freechips.rocketchip.diplomacy.TransferSizes
import scala.collection.mutable

// Checks the sanity of transactions exchanged on a *single* connection
class TLSanityChecker(params: TLBundleParameters, sparam: TLSlaveParameters, mparam: TLMasterParameters) {

  // Internal state mapping source -> state
  // States: 0 (Idle), 1 (pending AccessAck), 2 (pending HintAck), 3 (pending Grant), 4 (pending GrantAck), 5 (pending ReleaseAck),
  //       : -X (Remaining messages of a burst transaction), 1X (pending ProbeAck - X is previous state as Probes can interrupt existing transaction)
  // !!NOTE: Currently not keeping track of Acks, as concurrent transactions are possible. TODO convert to list of size = # concurrent transactions
  val sourceState = new mutable.HashMap[Int,Int]()

  // Internal state for burst parameter checking (source -> head of burst)
  val sourceBurst = new mutable.HashMap[Int,TLChannel]()
  val beatSize = log2Ceil(params.dataBits / 8)

  // Sanity checker
  def sanityCheck(txns: Seq[TLChannel]) : Unit = {
    for (txn <- txns) {
      txn match {
        case txna: TLBundleA =>
          if (txna.opcode.litValue() == 0) {

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

          } else if (txna.opcode.litValue() == 1) {

            // Assertions checking on first TLBundle
            assert(sparam.supportsPutPartial != TransferSizes.none, "(A) Channel does not support PUTPARTIAL requests.")
            assert(txna.param.litValue() == 0, "(A) Non-zero param field for PUTPARTIAL TLBundle")
            assert(containsLg(sparam.supportsPutPartial, txna.size), "(A) PUTPARTIAL Size is outside of valid transfer sizes")
            assert(alignedLg(txna.address, txna.size), s"(A) PUTPARTIAL Address (${txna.address}) is NOT aligned with size (${txna.size})")
            // TODO Check that high bits are aligned
            assert(!txna.corrupt.litToBoolean, "(A) Corrupt PUTPARTIAL TLBundle")

            // Burst helper function
            burstAHelper(txna)

          } else if (txna.opcode.litValue() == 2) {

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

          } else if (txna.opcode.litValue() == 3) {

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

          } else if (txna.opcode.litValue() == 4) {

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

          } else if (txna.opcode.litValue() == 5) {

            assert(sparam.supportsHint != TransferSizes.none, "(A) Channel does not support INTENT requests.")
            assert(txna.param.litValue() >= 0 && txna.param.litValue() <= 1, s"(A) Non-valid PARAM (${txna.param}) for INTENT Data Bundle")
            assert(containsLg(sparam.supportsHint, txna.size), "(A) INTENT Size is outside of valid transfer sizes")
            if (txna.size.litValue() < beatSize) {
              assert(alignedMaskLg(txna.mask, txna.size), s"(A) INTENT Mask (${txna.mask}) is not aligned with size (${txna.size})")
            } else {
              assert(alignedMaskLg(txna.mask, beatSize.U), s"(A) INTENT (Burst) Mask (${txna.mask}) is not aligned with beat size ($beatSize)")
            }
            assert(!txna.corrupt.litToBoolean, "(A) Corrupt INTENT TLBundle")

          } else if (txna.opcode.litValue() == 6) {

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

          } else if (txna.opcode.litValue() == 7) {

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

          } else {
            assert(false, "(A) Invalid OPCODE on A Channel")
          }

        case txnb: TLBundleB =>
          // NOTE: Ignoring checking TL-UL/UH messages forwarded on B channel
          if (txnb.opcode.litValue() == 6) {

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

          } else if (txnb.opcode.litValue() == 7) {

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

          } else {
            assert(false, "(B) Invalid OPCODE on B Channel")
          }

        case txnc: TLBundleC =>
          // NOTE: Ignoring checking TL-UL/UH messages forwarded on C channel
          if (txnc.opcode.litValue() == 5) {

            // Assertions checking on first TLBundle
            assert(mparam.supports.probe != TransferSizes.none, "(C) Channel does not support PROBEACKDATA requests.")
            assert(txnc.param.litValue() >= 0 && txnc.param.litValue() < 6, s"(C) Non-valid PARAM (${txnc.param}) for PROBEACKDATA Bundle")
            assert(alignedLg(txnc.address, txnc.size), s"(C) PROBEACKDATA Address (${txnc.address}) is NOT aligned with size (${txnc.size})")

            // Burst helper function
            burstCHelper(txnc)

          } else if (txnc.opcode.litValue() == 6) {

            assert(sparam.supportsAcquireB != TransferSizes.none, "(C) Channel does not support AcquireB, and thus RELEASE, requests.")
            assert(txnc.param.litValue() >= 0 && txnc.param.litValue() < 6, s"(B) Non-valid PARAM (${txnc.param}) for RELEASE Bundle")
            assert(!txnc.corrupt.litToBoolean, "(C) Corrupt RELEASE TLBundle")

          } else if (txnc.opcode.litValue() == 7) {

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
          if (txnd.opcode.litValue() == 0) {

            assert(txnd.param.litValue() == 0, "(D) Non-zero param field for ACCESSACK TLBundle")
            assert(!txnd.corrupt.litToBoolean, "(D) Non-zero corrupt field for ACCESSACK TLBundle")

          } else if (txnd.opcode.litValue() == 1) {

            // Assertions checking on first TLBundle
            assert(txnd.param.litValue() == 0, "(D) Non-zero param field for ACCESSACKDATA TLBundle")
            if (txnd.denied.litToBoolean) {
              assert(txnd.corrupt.litToBoolean, "(D) ACCESSACKDATA denied but not corrupt")
            }

            // Burst helper function
            burstDHelper(txnd)

          } else if (txnd.opcode.litValue() == 2) {

            assert(txnd.param.litValue() == 0, "(D) Non-zero param field for HINTACK TLBundle")
            assert(!txnd.corrupt.litToBoolean, "(D) Non-zero corrupt field for HINTACK TLBundle")

          } else if (txnd.opcode.litValue() == 4) {

            assert(txnd.param.litValue() >= 0 && txnd.param.litValue() < 3, "(D) Non-valid param field for GRANT TLBundle")
            assert(!txnd.corrupt.litToBoolean, "(D) Non-zero corrupt field for GRANT TLBundle")

          } else if (txnd.opcode.litValue() == 5) {

            // Assertions checking on first TLBundle
            assert(txnd.param.litValue() >= 0 && txnd.param.litValue() < 3, "(D) Non-valid param field for GRANTDATA TLBundle")
            if (txnd.denied.litToBoolean) {
              assert(txnd.corrupt.litToBoolean, "(D) GRANTDATA denied but not corrupt")
            }

            // Burst helper function
            burstDHelper(txnd)

          } else if (txnd.opcode.litValue() == 6) {

            assert(txnd.param.litValue() == 0, "(D) Non-zero param field for RELEASEACK TLBundle")
            assert(!txnd.corrupt.litToBoolean, "(D) Corrupt RELEASEACK TLBundle")
            assert(!txnd.denied.litToBoolean, "(D) RELEASEACK cannot be denied.")

          } else {
            assert(false, "(D) Invalid OPCODE on D Channel")
          }
        case _: TLBundleE =>
          // No asserts to check
      }
    }
  }

  // Helper Functions for Burst Checking
  def burstAHelper(txna: TLBundleA): Unit = {
    // If bundles are in a burst
    val source = txna.source.litValue().toInt
    if (!isNonBurst(txna) && txna.size.litValue() > beatSize) {
      if (sourceState.getOrElse(source,0) == 0) {
        // Start of burst
        sourceBurst(source) = txna
        sourceState(source) = 1 - (1 << (txna.size.litValue().toInt - beatSize))
      } else {
        // Checking constant parameters with head of burst
        val head = sourceBurst(source)
        head match {
          case heada: TLBundleA =>
            var result = true
            result &= txna.param.litValue() == heada.param.litValue()
            result &= txna.size.litValue() == heada.size.litValue()
            result &= txna.source.litValue() == heada.source.litValue()
            result &= txna.address.litValue() == heada.address.litValue()
            assert(result, s"(A) A constant field was modified within a beat. Burst head: ${sourceBurst(source)}. Given beat: $txna")

            sourceState(source) += 1
          case _ =>
            assert(false, s"Expected remaining ${0 - sourceState(source)} beats of burst. Burst head: ${sourceBurst(source)}. Given beat: $txna")
        }
      }
    } else if (sourceState.getOrElse(source,0) < 0) {
      assert(false, s"Expected remaining ${0 - sourceState(source)} beats of burst. Burst head: ${sourceBurst(source)}. Given beat: $txna")
    }
  }

  def burstCHelper(txnc: TLBundleC): Unit = {
    // If bundles are in a burst
    val source = txnc.source.litValue().toInt
    if (!isNonBurst(txnc) && txnc.size.litValue() > beatSize) {
      if (sourceState.getOrElse(source,0) == 0) {
        // Start of burst
        sourceBurst(source) = txnc
        sourceState(source) = 1 - (1 << (txnc.size.litValue().toInt - beatSize))
      } else {
        // Checking constant parameters with head of burst
        val head = sourceBurst(source)
        head match {
          case headc: TLBundleC =>
            var result = true
            result &= txnc.param.litValue() == headc.param.litValue()
            result &= txnc.size.litValue() == headc.size.litValue()
            result &= txnc.source.litValue() == headc.source.litValue()
            result &= txnc.address.litValue() == headc.address.litValue()
            assert(result, s"(C) A constant field was modified within a beat. Burst head: ${sourceBurst(source)}. Given beat: $txnc")

            sourceState(source) += 1
          case _ =>
            assert(false, s"Expected remaining ${0 - sourceState(source)} beats of burst. Burst head: ${sourceBurst(source)}. Given beat: $txnc")
        }
      }
    } else if (sourceState.getOrElse(source,0) < 0) {
      assert(false, s"Expected remaining ${0 - sourceState(source)} beats of burst. Burst head: ${sourceBurst(source)}. Given beat: $txnc")
    }
  }

  def burstDHelper(txnd: TLBundleD): Unit = {
    // If bundles are in a burst
    val source = txnd.source.litValue().toInt
    if (!isNonBurst(txnd) && txnd.size.litValue() > beatSize) {
      if (sourceState.getOrElse(source,0) == 0) {
        // Start of burst
        sourceBurst(source) = txnd
        sourceState(source) = 1 - (1 << (txnd.size.litValue().toInt - beatSize))
      } else {
        // Checking constant parameters with head of burst
        val head = sourceBurst(source)
        head match {
          case headd: TLBundleD =>
            var result = true
            result &= txnd.param.litValue() == headd.param.litValue()
            result &= txnd.size.litValue() == headd.size.litValue()
            result &= txnd.source.litValue() == headd.source.litValue()
            result &= txnd.sink.litValue() == headd.sink.litValue()
            result &= txnd.denied.litValue() == headd.denied.litValue()
            assert(result, s"(D) A constant field was modified within a beat. Burst head: ${sourceBurst(source)}. Given beat: $txnd")

            sourceState(source) += 1
          case _ =>
            assert(false, s"Expected remaining ${0 - sourceState(source)} beats of burst. Burst head: ${sourceBurst(source)}. Given beat: $txnd")
        }
      }
    } else if (sourceState.getOrElse(source,0) < 0) {
      assert(false, s"Expected remaining ${0 - sourceState(source)} beats of burst. Burst head: ${sourceBurst(source)}. Given beat: $txnd")
    }
  }
}