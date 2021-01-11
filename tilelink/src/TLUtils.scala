package verif

import chisel3._
import chisel3.util.isPow2
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import verif.TLTransaction._

import scala.collection.mutable.{ListBuffer, HashMap}
import scala.math.ceil

package object TLUtils {
  // Temporary location for parameters
  def standaloneSlaveParams: TLSlavePortParameters = TLSlavePortParameters.v1(Seq(TLSlaveParameters.v1(address = Seq(AddressSet(0x0, 0xfff)),
    supportsGet = TransferSizes(1, 32), supportsPutFull = TransferSizes(1, 32), supportsPutPartial = TransferSizes(1, 32),
    supportsLogical = TransferSizes(1, 32), supportsArithmetic = TransferSizes(1, 32), supportsHint = TransferSizes(1, 32),
    regionType = RegionType.UNCACHED)),
    beatBytes = 8)
  def standaloneMasterParams: TLMasterPortParameters = TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = "bundleBridgetoTL")))
  //    supportsProbe = TransferSizes(1, 32),
  //    supportsGet = TransferSizes(1, 32), supportsPutFull = TransferSizes(1, 32), supportsPutPartial = TransferSizes(1, 32),
  //    supportsLogical = TransferSizes(1, 32), supportsArithmetic = TransferSizes(1, 32), supportsHint = TransferSizes(1, 32))))
  def verifTLBundleParams: TLBundleParameters = TLBundleParameters(standaloneMasterParams, standaloneSlaveParams)
  // Temporary cache parameters
  def standaloneSlaveParamsC: TLSlavePortParameters = TLSlavePortParameters.v1(Seq(TLSlaveParameters.v1(address = Seq(AddressSet(0x0, 0xfff)),
    supportsGet = TransferSizes(1, 32), supportsPutFull = TransferSizes(1, 32), supportsPutPartial = TransferSizes(1, 32),
    supportsLogical = TransferSizes(1, 32), supportsArithmetic = TransferSizes(1, 32), supportsHint = TransferSizes(1, 32),
    supportsAcquireB = TransferSizes(1, 32), supportsAcquireT = TransferSizes(1, 32),
    regionType = RegionType.UNCACHED)),
    endSinkId = 1, beatBytes = 8)
  def standaloneMasterParamsC: TLMasterPortParameters = TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = "TestBundle",
    supportsProbe = TransferSizes(1, 32), supportsGet = TransferSizes(1, 32), supportsPutFull = TransferSizes(1, 32),
    supportsPutPartial = TransferSizes(1, 32), supportsLogical = TransferSizes(1, 32), supportsArithmetic = TransferSizes(1, 32),
    supportsHint = TransferSizes(1, 32))))
  def verifTLBundleParamsC: TLBundleParameters = TLBundleParameters(standaloneMasterParamsC, standaloneSlaveParamsC)

  // Helper functions for message checking
  def aligned(data : UInt, base : UInt) : Boolean = {
    val dataI = data.litValue()
    val baseI = base.litValue() - 1
    ((dataI & baseI) == 0) && contiguous(baseI.U)
  }

  def alignedLg(data : UInt, base : UInt) : Boolean = {
    aligned(data, (1 << base.litValue().toInt).U)
  }

  def alignedMaskLg(mask : UInt, size : UInt) : Boolean = {
    (1 << (1 << size.litValue().toInt)) - 1 == mask.litValue().toInt
  }

  def contiguous(data : UInt) : Boolean = {
    val dataI = data.litValue()
    ((dataI + 1) & ~dataI) == (dataI + 1)
  }

  def contains(sizes: TransferSizes, x: UInt) : Boolean = {
    (x.litValue() >= sizes.min && x.litValue() <= sizes.max && isPow2(x.litValue()))
  }

  def containsLg(sizes: TransferSizes, lg: UInt) : Boolean = {
    contains(sizes, (1 << lg.litValue().toInt).U)
  }


  // Helper method to get size of TLChannel
  def getTLBundleDataSizeBytes (bnd : TLChannel): Int = {
    bnd match {
      case _: TLBundleA =>
        val bndc = bnd.asInstanceOf[TLBundleA]
        1 << bndc.size.litValue().toInt
      case _: TLBundleB =>
        val bndc = bnd.asInstanceOf[TLBundleB]
        1 << bndc.size.litValue().toInt
      case _: TLBundleC =>
        val bndc = bnd.asInstanceOf[TLBundleC]
        1 << bndc.size.litValue().toInt
      case _: TLBundleD =>
        val bndc = bnd.asInstanceOf[TLBundleD]
        1 << bndc.size.litValue().toInt
      case _: TLBundleE =>
        1
    }
  }

  // Helper method to see if transaction is non-Burst
  def isNonBurst (bnd : TLChannel): Boolean = {
    bnd match {
      case _: TLBundleA =>
        val bndc = bnd.asInstanceOf[TLBundleA]
        // Get, AcquireBlock, AcquirePerm
        if (bndc.opcode.litValue() == 4 || bndc.opcode.litValue() == 5 || bndc.opcode.litValue() == 6 || bndc.opcode.litValue() == 7) {
          return true
        }
        false
      case _: TLBundleB =>
        val bndc = bnd.asInstanceOf[TLBundleB]
        // Get, ProbeBlock, ProbePerm
        if (bndc.opcode.litValue() == 4 || bndc.opcode.litValue() == 5 || bndc.opcode.litValue() == 6 || bndc.opcode.litValue() == 7) {
          return true
        }
        false
      case _: TLBundleC =>
        val bndc = bnd.asInstanceOf[TLBundleC]
        // AccessAck, ProbeAck, Release
        if (bndc.opcode.litValue() == 0 || bndc.opcode.litValue() == 4 || bndc.opcode.litValue() == 5) {
          return true
        }
        false
      case _: TLBundleD =>
        val bndc = bnd.asInstanceOf[TLBundleD]
        // AccessAck, Grant, ReleaseAck
        if (bndc.opcode.litValue() == 0 || bndc.opcode.litValue() == 4 || bndc.opcode.litValue() == 6) {
          return true
        }
        false
      case _: TLBundleE =>
        // Always single message
        true
    }
  }

  // Helper function to map size -> # of beats
  def sizeToBeats (size : UInt, beatBytes : Int = 8): Int = {
    val sizeInt = 1 << size.litValue().toInt
    ceil(sizeInt / beatBytes.toDouble).toInt
  }

  // Helper method to test if given TLBundles (TLChannels) make up a complete TLTransaction
  def isCompleteTLTxn (txns: List[TLChannel]) : Boolean = {
    // Edge case
    if (txns.isEmpty) return false

    // TODO Hardcoded, update when configurability is added
    val beatBytes = 8
    val expectedTxnCount = if (isNonBurst(txns.head)) 1 else
      ceil(getTLBundleDataSizeBytes(txns.head) / beatBytes.toDouble).toInt

    // Currently only checks count
    // TODO Add checks for consistency (size, source, opcode, etc)
    txns.length == expectedTxnCount
  }

  // Helper method to group together burst TLBundles
  def groupTLBundles (txns: List[TLChannel]) : List[List[TLChannel]] = {
    // TODO Hardcoded for now, update when configurability is added
    val beatBytes = 8
    val txnsLB = new ListBuffer[TLChannel]
    txnsLB ++= txns
    var result = new ListBuffer[List[TLChannel]]

    while (txnsLB.nonEmpty) {
      val txnCount = if (isNonBurst(txnsLB.head)) 1 else ceil(getTLBundleDataSizeBytes(txnsLB.head) / beatBytes.toDouble).toInt

      // Grouping txnCount TLTransactions (of the same type)
      val newList = new ListBuffer[TLChannel]
      var index = 0
      newList += txnsLB.remove(index)
      // Getting Class as "type"
      val classType = newList.head.getClass
      while (newList.length < txnCount) {
        if (txnsLB(index).getClass.equals(classType)) {
          newList += txnsLB.remove(index)
        } else {
          index += 1
        }
      }

      result += newList.toList
    }

    result.toList
  }

  // TODO Correct MASK assertion checking
  // TODO: move all checks into the TLMonitor
  /*
  def TLBundlestoTLTransaction(bnds: List[TLChannel], params: TLBundleParameters) : TLTransaction[_] = {
    // Currently Hardcoding Parameters
    val TLSParam: TLSlaveParameters = standaloneSlaveParamsC.managers(0)
    val TLMParam = standaloneMasterParamsC.clients(0)
    val beatSize = 3 // beatBytes = 8

    val bndsq = new Queue[TLChannel]()
    bndsq ++= bnds
    val bnd = bndsq.dequeue()

    bnd match {
      case _: TLBundleA =>
        val bndc = bnd.asInstanceOf[TLBundleA]
        if (bndc.opcode.litValue() == 0) {

          // Assertions checking on first TLBundle
          assert(TLSParam.supportsPutFull != TransferSizes.none, "(A) Channel does not support PUTFULL requests.")
          assert(bndc.param.litValue() == 0, "(A) Non-zero param field for PUTFULL TLBundle")
          assert(containsLg(TLSParam.supportsPutFull, bndc.size), "(A) PUTFULL Size is outside of valid transfer sizes")
          assert(alignedLg(bndc.address, bndc.size), s"(A) PUTFULL Address (${bndc.address}) is NOT aligned with size (${bndc.size})")
          if (bndc.size.litValue() < beatSize) {
            assert(alignedMaskLg(bndc.mask, bndc.size), s"(A) PUTFULL Mask (${bndc.mask}) is not aligned with size (${bndc.size})")
          } else {
            assert(alignedMaskLg(bndc.mask, beatSize.U), s"(A) PUTFULL (Burst) Mask (${bndc.mask}) is not aligned with beat size ($beatSize)")
          }
          assert(contiguous(bndc.mask), "(A) PUTFULL MASK is not contiguous")
          assert(!bndc.corrupt.litToBoolean, "(A) Corrupt PUTFULL TLBundle")

          // If bundles are in a burst
          if (bndsq.size > 0) {
            var masks = new ListBuffer[UInt]
            var datas = new ListBuffer[UInt]
            masks += bndc.mask
            datas += bndc.data

            while (bndsq.nonEmpty) {
              // Checking if other bundles in burst are valid
              val other_bndc = bndsq.dequeue().asInstanceOf[TLBundleA]

              assert(bndc.address.litValue() == other_bndc.address.litValue())
              assert(bndc.param.litValue() == other_bndc.param.litValue())
              assert(bndc.size.litValue() == other_bndc.size.litValue())

              masks += other_bndc.mask
              datas += other_bndc.data
            }
            Put(size = bndc.size, source = bndc.source, addr = bndc.address, masks = masks.toList, datas = datas.toList)
          } else {
            PutFull(source = bndc.source, addr = bndc.address, mask = bndc.mask, data = bndc.data)
          }
        } else if (bndc.opcode.litValue() == 1) {

          // Assertions checking on first TLBundle
          assert(TLSParam.supportsPutPartial != TransferSizes.none, "(A) Channel does not support PUTPARTIAL requests.")
          assert(bndc.param.litValue() == 0, "(A) Non-zero param field for PUTPARTIAL TLBundle")
          assert(containsLg(TLSParam.supportsPutPartial, bndc.size), "(A) PUTPARTIAL Size is outside of valid transfer sizes")
          assert(alignedLg(bndc.address, bndc.size), s"(A) PUTPARTIAL Address (${bndc.address}) is NOT aligned with size (${bndc.size})")
          // TODO Check that high bits are aligned
          assert(!bndc.corrupt.litToBoolean, "(A) Corrupt PUTPARTIAL TLBundle")

          // If bundles are in a burst
          if (bndsq.size > 0) {
            var masks = new ListBuffer[UInt]
            var datas = new ListBuffer[UInt]
            masks += bndc.mask
            datas += bndc.data

            while (bndsq.nonEmpty) {
              // Checking if other bundles in burst are valid
              val other_bndc = bndsq.dequeue().asInstanceOf[TLBundleA]

              assert(bndc.address.litValue() == other_bndc.address.litValue())
              assert(bndc.param.litValue() == other_bndc.param.litValue())
              assert(bndc.size.litValue() == other_bndc.size.litValue())

              masks += other_bndc.mask
              datas += other_bndc.data
            }
            PutPartialBurst(size = bndc.size, source = bndc.source, addr = bndc.address, masks = masks.toList, datas = datas.toList)
          } else {
            PutPartial(source = bndc.source, addr = bndc.address, mask = bndc.mask, data = bndc.data)
          }

        } else if (bndc.opcode.litValue() == 2) {

          // Assertions checking on first TLBundle
          assert(TLSParam.supportsArithmetic != TransferSizes.none, "(A) Channel does not support ARITHMETIC requests.")
          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() <= 4, s"(A) Non-valid PARAM (${bndc.param}) for ARITHMETIC Data Bundle")
          assert(containsLg(TLSParam.supportsArithmetic, bndc.size), "(A) ARITHMETIC Size is outside of valid transfer sizes")
          assert(alignedLg(bndc.address, bndc.size), s"(A) ARITHMETIC Address (${bndc.address}) is NOT aligned with size (${bndc.size})")
          if (bndc.size.litValue() < beatSize) {
            assert(alignedMaskLg(bndc.mask, bndc.size), s"(A) ARITHMETIC Mask (${bndc.mask}) is not aligned with size (${bndc.size})")
          } else {
            assert(alignedMaskLg(bndc.mask, beatSize.U), s"(A) ARITHMETIC (Burst) Mask (${bndc.mask}) is not aligned with beat size ($beatSize)")
          }
          assert(contiguous(bndc.mask), "(A) ARITHMETIC MASK is not contiguous")
          assert(!bndc.corrupt.litToBoolean, "(A) Corrupt ARITHMETIC TLBundle")

          // If bundles are in a burst
          if (bndsq.size > 0) {
            var masks = new ListBuffer[UInt]
            var datas = new ListBuffer[UInt]
            masks += bndc.mask
            datas += bndc.data

            while (bndsq.nonEmpty) {
              // Checking if other bundles in burst are valid
              val other_bndc = bndsq.dequeue().asInstanceOf[TLBundleA]

              assert(bndc.address.litValue() == other_bndc.address.litValue())
              assert(bndc.param.litValue() == other_bndc.param.litValue())
              assert(bndc.size.litValue() == other_bndc.size.litValue())

              masks += other_bndc.mask
              datas += other_bndc.data
            }
            ArithDataBurst(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address, masks = masks.toList,
              datas = datas.toList)
          } else {
            ArithData(param = bndc.param, source = bndc.source, addr = bndc.address, mask = bndc.mask, data = bndc.data)
          }

        } else if (bndc.opcode.litValue() == 3) {

          // Assertions checking on first TLBundle
          assert(TLSParam.supportsLogical != TransferSizes.none, "(A) Channel does not support LOGIC requests.")
          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() <= 3, s"(A) Non-valid PARAM (${bndc.param}) for LOGIC Data Bundle")
          assert(containsLg(TLSParam.supportsLogical, bndc.size), "(A) LOGIC Size is outside of valid transfer sizes")
          assert(alignedLg(bndc.address, bndc.size), s"(A) LOGIC Address (${bndc.address}) is NOT aligned with size (${bndc.size})")
          if (bndc.size.litValue() < beatSize) {
            assert(alignedMaskLg(bndc.mask, bndc.size), s"(A) LOGIC Mask (${bndc.mask}) is not aligned with size (${bndc.size})")
          } else {
            assert(alignedMaskLg(bndc.mask, beatSize.U), s"(A) LOGIC (Burst) Mask (${bndc.mask}) is not aligned with beat size ($beatSize)")
          }
          assert(contiguous(bndc.mask), "(A) LOGICAL MASK is not contiguous")
          assert(!bndc.corrupt.litToBoolean, "(A) Corrupt LOGICAL TLBundle")

          // If bundles are in a burst
          if (bndsq.size > 0) {
            var masks = new ListBuffer[UInt]
            var datas = new ListBuffer[UInt]
            masks += bndc.mask
            datas += bndc.data

            while (bndsq.nonEmpty) {
              // Checking if other bundles in burst are valid
              val other_bndc = bndsq.dequeue().asInstanceOf[TLBundleA]

              assert(bndc.address.litValue() == other_bndc.address.litValue())
              assert(bndc.param.litValue() == other_bndc.param.litValue())
              assert(bndc.size.litValue() == other_bndc.size.litValue())

              masks += other_bndc.mask
              datas += other_bndc.data
            }
            LogicDataBurst(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address, masks = masks.toList,
              datas = datas.toList)
          } else {
            LogicData(param = bndc.param, source = bndc.source, addr = bndc.address, mask = bndc.mask, data = bndc.data)
          }

        } else if (bndc.opcode.litValue() == 4) {

          // Assertions checking on first TLBundle
          assert(TLSParam.supportsGet != TransferSizes.none, "(A) Channel does not support GET requests.")
          assert(bndc.param.litValue() == 0, "(A) Non-zero param field for GET TLBundle")
          assert(containsLg(TLSParam.supportsGet, bndc.size), "(A) GET Size is outside of valid transfer sizes")
          if (bndc.size.litValue() < beatSize) {
            assert(alignedMaskLg(bndc.mask, bndc.size), s"(A) GET Mask (${bndc.mask}) is not aligned with size (${bndc.size})")
          } else {
            assert(alignedMaskLg(bndc.mask, beatSize.U), s"(A) GET (Burst) Mask (${bndc.mask}) is not aligned with beat size ($beatSize)")
          }
          assert(contiguous(bndc.mask), "(A) GET MASK is not contiguous")
          assert(!bndc.corrupt.litToBoolean, "(A) Corrupt GET TLBundle")
          Get(size = bndc.size, source = bndc.source, mask = bndc.mask, addr = bndc.address)

        } else if (bndc.opcode.litValue() == 5) {

          assert(TLSParam.supportsHint != TransferSizes.none, "(A) Channel does not support INTENT requests.")
          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() <= 1, s"(A) Non-valid PARAM (${bndc.param}) for INTENT Data Bundle")
          assert(containsLg(TLSParam.supportsHint, bndc.size), "(A) INTENT Size is outside of valid transfer sizes")
          if (bndc.size.litValue() < beatSize) {
            assert(alignedMaskLg(bndc.mask, bndc.size), s"(A) INTENT Mask (${bndc.mask}) is not aligned with size (${bndc.size})")
          } else {
            assert(alignedMaskLg(bndc.mask, beatSize.U), s"(A) INTENT (Burst) Mask (${bndc.mask}) is not aligned with beat size ($beatSize)")
          }
          assert(!bndc.corrupt.litToBoolean, "(A) Corrupt INTENT TLBundle")

          Intent(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address, mask = bndc.mask)

        } else if (bndc.opcode.litValue() == 6) {

          assert(TLSParam.supportsAcquireB != TransferSizes.none, "(A) Channel does not support AcquireB requests.")
          assert(TLSParam.supportsAcquireT != TransferSizes.none, "(A) Channel does not support AcquireT requests.")
          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() <= 3, s"(A) Non-valid PARAM (${bndc.param}) for ACQUIREBLOCK Bundle")
          assert(containsLg(TLSParam.supportsAcquireT, bndc.size), "(A) ACQUIRE Size is outside of valid transfer sizes")
          if (bndc.size.litValue() < beatSize) {
            assert(alignedMaskLg(bndc.mask, bndc.size), s"(A) ACQUIREBLOCK Mask (${bndc.mask}) is not aligned with size (${bndc.size})")
          } else {
            assert(alignedMaskLg(bndc.mask, beatSize.U), s"(A) ACQUIREBLOCK (Burst) Mask (${bndc.mask}) is not aligned with beat size ($beatSize)")
          }
          assert(contiguous(bndc.mask), "(A) ACQUIREBLOCK MASK is not contiguous")
          assert(!bndc.corrupt.litToBoolean, "(A) Corrupt ACQUIREBLOCK TLBundle")

          AcquireBlock(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address, mask = bndc.mask)

        } else if (bndc.opcode.litValue() == 7) {

          assert(TLSParam.supportsAcquireB != TransferSizes.none, "(A) Channel does not support AcquireB requests.")
          assert(TLSParam.supportsAcquireT != TransferSizes.none, "(A) Channel does not support AcquireT requests.")
          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() <= 3, s"(A) Non-valid PARAM (${bndc.param}) for ACQUIREPERM Bundle")
          assert(containsLg(TLSParam.supportsAcquireT, bndc.size), "(A) ACQUIRE Size is outside of valid transfer sizes")
          if (bndc.size.litValue() < beatSize) {
            assert(alignedMaskLg(bndc.mask, bndc.size), s"(A) ACQUIREPERM Mask (${bndc.mask}) is not aligned with size (${bndc.size})")
          } else {
            assert(alignedMaskLg(bndc.mask, beatSize.U), s"(A) ACQUIREPERM (Burst) Mask (${bndc.mask}) is not aligned with beat size ($beatSize)")
          }
          assert(contiguous(bndc.mask), "(A) LOGICAL MASK is not contiguous")
          assert(!bndc.corrupt.litToBoolean, "(A) Corrupt ACQUIREPERM TLBundle")

          AcquirePerm(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address, mask = bndc.mask)

        } else {

          assert(false, "(A) Invalid OPCODE on A Channel")
          Get(size = 0.U, source = 0.U, mask = 0.U, addr = 0.U)

        }
      case _: TLBundleB =>
        val bndc = bnd.asInstanceOf[TLBundleB]
        if (bndc.opcode.litValue() == 0) {

          // Assertions checking on first TLBundle
          assert(TLSParam.supportsPutFull != TransferSizes.none, "(B) Channel does not support PUTFULL requests.")
          assert(bndc.param.litValue() == 0, "(B) Non-zero param field for PUTFULL TLBundle")
          assert(containsLg(TLSParam.supportsPutFull, bndc.size), "(B) PUTFULL Size is outside of valid transfer sizes")
          assert(alignedLg(bndc.address, bndc.size), s"(B) PUTFULL Address (${bndc.address}) is NOT aligned with size (${bndc.size})")
          if (bndc.size.litValue() < beatSize) {
            assert(alignedMaskLg(bndc.mask, bndc.size), s"(B) PUTFULL Mask (${bndc.mask}) is not aligned with size (${bndc.size})")
          } else {
            assert(alignedMaskLg(bndc.mask, beatSize.U), s"(B) PUTFULL (Burst) Mask (${bndc.mask}) is not aligned with beat size ($beatSize)")
          }
          assert(contiguous(bndc.mask), "(B) PUTFULL MASK is not contiguous")
          assert(!bndc.corrupt.litToBoolean, "(B) Corrupt PUTFULL TLBundle")

          // If bundles are in a burst
          if (bndsq.size > 0) {
            var masks = new ListBuffer[UInt]
            var datas = new ListBuffer[UInt]
            masks += bndc.mask
            datas += bndc.data

            while (bndsq.nonEmpty) {
              // Checking if other bundles in burst are valid
              val other_bndc = bndsq.dequeue().asInstanceOf[TLBundleA]

              assert(bndc.address.litValue() == other_bndc.address.litValue())
              assert(bndc.param.litValue() == other_bndc.param.litValue())
              assert(bndc.size.litValue() == other_bndc.size.litValue())

              masks += other_bndc.mask
              datas += other_bndc.data
            }
            PutFullBurst(size = bndc.size, source = bndc.source, addr = bndc.address, masks = masks.toList, datas = datas.toList, fwd = true.B)
          } else {
            PutFull(source = bndc.source, addr = bndc.address, mask = bndc.mask, data = bndc.data, fwd = true.B)
          }
        } else if (bndc.opcode.litValue() == 1) {

          // Assertions checking on first TLBundle
          assert(TLSParam.supportsPutPartial != TransferSizes.none, "(B) Channel does not support PUTPARTIAL requests.")
          assert(bndc.param.litValue() == 0, "(B) Non-zero param field for PUTPARTIAL TLBundle")
          assert(containsLg(TLSParam.supportsPutPartial, bndc.size), "(B) PUTPARTIAL Size is outside of valid transfer sizes")
          assert(alignedLg(bndc.address, bndc.size), s"(B) PUTPARTIAL Address (${bndc.address}) is NOT aligned with size (${bndc.size})")
          // TODO Check that high bits are aligned
          assert(!bndc.corrupt.litToBoolean, "(B) Corrupt PUTPARTIAL TLBundle")

          // If bundles are in a burst
          if (bndsq.size > 0) {
            var masks = new ListBuffer[UInt]
            var datas = new ListBuffer[UInt]
            masks += bndc.mask
            datas += bndc.data

            while (bndsq.nonEmpty) {
              // Checking if other bundles in burst are valid
              val other_bndc = bndsq.dequeue().asInstanceOf[TLBundleA]

              assert(bndc.address.litValue() == other_bndc.address.litValue())
              assert(bndc.param.litValue() == other_bndc.param.litValue())
              assert(bndc.size.litValue() == other_bndc.size.litValue())

              masks += other_bndc.mask
              datas += other_bndc.data
            }
            PutPartialBurst(size = bndc.size, source = bndc.source, addr = bndc.address, masks = masks.toList, datas = datas.toList, fwd = true.B)
          } else {
            PutPartial(source = bndc.source, addr = bndc.address, mask = bndc.mask, data = bndc.data, fwd = true.B)
          }

        } else if (bndc.opcode.litValue() == 2) {

          // Assertions checking on first TLBundle
          assert(TLSParam.supportsArithmetic != TransferSizes.none, "(B) Channel does not support ARITHMETIC requests.")
          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() <= 4, s"(B) Non-valid PARAM (${bndc.param}) for ARITHMETIC Data Bundle")
          assert(containsLg(TLSParam.supportsArithmetic, bndc.size), "(B) ARITHMETIC Size is outside of valid transfer sizes")
          assert(alignedLg(bndc.address, bndc.size), s"(B) ARITHMETIC Address (${bndc.address}) is NOT aligned with size (${bndc.size})")
          if (bndc.size.litValue() < beatSize) {
            assert(alignedMaskLg(bndc.mask, bndc.size), s"(B) ARITHMETIC Mask (${bndc.mask}) is not aligned with size (${bndc.size})")
          } else {
            assert(alignedMaskLg(bndc.mask, beatSize.U), s"(B) ARITHMETIC (Burst) Mask (${bndc.mask}) is not aligned with beat size ($beatSize)")
          }
          assert(contiguous(bndc.mask), "(B) ARITHMETIC MASK is not contiguous")
          assert(!bndc.corrupt.litToBoolean, "(B) Corrupt ARITHMETIC TLBundle")

          // If bundles are in a burst
          if (bndsq.size > 0) {
            var masks = new ListBuffer[UInt]
            var datas = new ListBuffer[UInt]
            masks += bndc.mask
            datas += bndc.data

            while (bndsq.nonEmpty) {
              // Checking if other bundles in burst are valid
              val other_bndc = bndsq.dequeue().asInstanceOf[TLBundleA]

              assert(bndc.address.litValue() == other_bndc.address.litValue())
              assert(bndc.param.litValue() == other_bndc.param.litValue())
              assert(bndc.size.litValue() == other_bndc.size.litValue())

              masks += other_bndc.mask
              datas += other_bndc.data
            }
            ArithDataBurst(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address, masks = masks.toList,
              datas = datas.toList, fwd = true.B)
          } else {
            ArithData(param = bndc.param, source = bndc.source, addr = bndc.address, mask = bndc.mask, data = bndc.data, fwd = true.B)
          }

        } else if (bndc.opcode.litValue() == 3) {

          // Assertions checking on first TLBundle
          assert(TLSParam.supportsLogical != TransferSizes.none, "(B) Channel does not support LOGIC requests.")
          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() <= 3, s"(B) Non-valid PARAM (${bndc.param}) for LOGIC Data Bundle")
          assert(containsLg(TLSParam.supportsLogical, bndc.size), "(B) LOGIC Size is outside of valid transfer sizes")
          assert(alignedLg(bndc.address, bndc.size), s"(B) LOGIC Address (${bndc.address}) is NOT aligned with size (${bndc.size})")
          if (bndc.size.litValue() < beatSize) {
            assert(alignedMaskLg(bndc.mask, bndc.size), s"(B) LOGIC Mask (${bndc.mask}) is not aligned with size (${bndc.size})")
          } else {
            assert(alignedMaskLg(bndc.mask, beatSize.U), s"(B) LOGIC (Burst) Mask (${bndc.mask}) is not aligned with beat size ($beatSize)")
          }
          assert(contiguous(bndc.mask), "(B) LOGIC MASK is not contiguous")
          assert(!bndc.corrupt.litToBoolean, "(B) Corrupt LOGICAL TLBundle")

          // If bundles are in a burst
          if (bndsq.size > 0) {
            var masks = new ListBuffer[UInt]
            var datas = new ListBuffer[UInt]
            masks += bndc.mask
            datas += bndc.data

            while (bndsq.nonEmpty) {
              // Checking if other bundles in burst are valid
              val other_bndc = bndsq.dequeue().asInstanceOf[TLBundleA]

              assert(bndc.address.litValue() == other_bndc.address.litValue())
              assert(bndc.param.litValue() == other_bndc.param.litValue())
              assert(bndc.size.litValue() == other_bndc.size.litValue())

              masks += other_bndc.mask
              datas += other_bndc.data
            }
            LogicDataBurst(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address, masks = masks.toList,
              datas = datas.toList, fwd = true.B)
          } else {
            LogicData(param = bndc.param, source = bndc.source, addr = bndc.address, mask = bndc.mask, data = bndc.data, fwd = true.B)
          }

        } else if (bndc.opcode.litValue() == 4) {

          // Assertions checking on first TLBundle
          assert(TLSParam.supportsGet != TransferSizes.none, "(B) Channel does not support GET requests.")
          assert(bndc.param.litValue() == 0, "(B) Non-zero param field for GET TLBundle")
          assert(containsLg(TLSParam.supportsGet, bndc.size), "(B) GET Size is outside of valid transfer sizes")
          if (bndc.size.litValue() < beatSize) {
            assert(alignedMaskLg(bndc.mask, bndc.size), s"(B) GET Mask (${bndc.mask}) is not aligned with size (${bndc.size})")
          } else {
            assert(alignedMaskLg(bndc.mask, beatSize.U), s"(B) GET (Burst) Mask (${bndc.mask}) is not aligned with beat size ($beatSize)")
          }
          assert(contiguous(bndc.mask), "(B) GET MASK is not contiguous")
          assert(!bndc.corrupt.litToBoolean, "(B) Corrupt GET TLBundle")
          Get(size = bndc.size, source = bndc.source, mask = bndc.mask, addr = bndc.address, fwd = true.B)

        } else if (bndc.opcode.litValue() == 5) {

          // Assertions checking on first TLBundle
          assert(TLSParam.supportsHint != TransferSizes.none, "(B) Channel does not support INTENT requests.")
          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() < 2, s"(B) Non-valid PARAM (${bndc.param}) for INTENT Data Bundle")
          assert(containsLg(TLSParam.supportsHint, bndc.size), "(B) INTENT Size is outside of valid transfer sizes")
          if (bndc.size.litValue() < beatSize) {
            assert(alignedMaskLg(bndc.mask, bndc.size), s"(B) INTENT Mask (${bndc.mask}) is not aligned with size (${bndc.size})")
          } else {
            assert(alignedMaskLg(bndc.mask, beatSize.U), s"(B) INTENT (Burst) Mask (${bndc.mask}) is not aligned with beat size ($beatSize)")
          }
          assert(contiguous(bndc.mask), "(B) INTEN MASK is not contiguous")
          assert(!bndc.corrupt.litToBoolean, "(B) Corrupt INTENT TLBundle")

          Intent(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address, mask = bndc.mask, fwd = true.B)

        } else if (bndc.opcode.litValue() == 6) {

          // Assertions checking on first TLBundle
          assert(TLMParam.supports.probe != TransferSizes.none, "(B) Channel does not support PROBEBLOCK requests.")
          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() < 3, s"(B) Non-valid PARAM (${bndc.param}) for PROBEBLOCK Bundle")
          if (bndc.size.litValue() < beatSize) {
            assert(alignedMaskLg(bndc.mask, bndc.size), s"(B) PROBEBLOCK Mask (${bndc.mask}) is not aligned with size (${bndc.size})")
          } else {
            assert(alignedMaskLg(bndc.mask, beatSize.U), s"(B) PROBEBLOCK (Burst) Mask (${bndc.mask}) is not aligned with beat size ($beatSize)")
          }
          assert(contiguous(bndc.mask), "(B) PROBEBLOCK MASK is not contiguous")
          assert(!bndc.corrupt.litToBoolean, "(B) Corrupt PROBEBLOCK TLBundle")

          ProbeBlock(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address, mask = bndc.mask)

        } else if (bndc.opcode.litValue() == 7) {

          // Assertions checking on first TLBundle
          assert(TLMParam.supports.probe != TransferSizes.none, "(B) Channel does not support PROBEPERM requests.")
          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() < 3, s"(B) Non-valid PARAM (${bndc.param}) for PROBEPERM Bundle")
          if (bndc.size.litValue() < beatSize) {
            assert(alignedMaskLg(bndc.mask, bndc.size), s"(B) PROBEPERM Mask (${bndc.mask}) is not aligned with size (${bndc.size})")
          } else {
            assert(alignedMaskLg(bndc.mask, beatSize.U), s"(B) PROBEPERM (Burst) Mask (${bndc.mask}) is not aligned with beat size ($beatSize)")
          }
          assert(contiguous(bndc.mask), "(B) PROBEPERM MASK is not contiguous")
          assert(!bndc.corrupt.litToBoolean, "(B) Corrupt PROBEPERM TLBundle")

          Intent(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address, mask = bndc.mask)

        } else {

          assert(false, "(B) Invalid OPCODE on B Channel")
          Get(size = 0.U, source = 0.U, mask = 0.U, addr = 0.U)

        }
      case _: TLBundleC =>
        val bndc = bnd.asInstanceOf[TLBundleC]
        if (bndc.opcode.litValue() == 0) {

          assert(bndc.param.litValue() == 0, "(C) Non-zero param field for ACCESSACK TLBundle")
          assert(alignedLg(bndc.address, bndc.size), s"(B) ACCESSACK Address (${bndc.address}) is NOT aligned with size (${bndc.size})")
          assert(!bndc.corrupt.litToBoolean, "(C) Corrupt ACCESSACK TLBundle")
          AccessAck(size = bndc.size, source = bndc.source, addr = bndc.address, denied = false.B, fwd = true.B)

        } else if (bndc.opcode.litValue() == 1) {

          // Assertions checking on first TLBundle
          assert(bndc.param.litValue() == 0, "(C) Non-zero param field for ACCESSACKDATA TLBundle")
          assert(alignedLg(bndc.address, bndc.size), s"(B) ACCESSACKData Address (${bndc.address}) is NOT aligned with size (${bndc.size})")

          if (bndsq.size > 0) {
            var datas = new ListBuffer[UInt]
            datas += bndc.data

            while (bndsq.nonEmpty) {
              // Checking if other bundles in burst are valid
              val other_bndc = bndsq.dequeue().asInstanceOf[TLBundleC]

              assert(bndc.param.litValue() == other_bndc.param.litValue())
              assert(bndc.size.litValue() == other_bndc.size.litValue())
              assert(bndc.address.litValue() == other_bndc.address.litValue())

              datas += other_bndc.data
            }
            AccessAckDataBurst(size = bndc.size, source = bndc.source, denied = false.B, addr = bndc.address, datas = datas.toList, fwd = true.B)
          } else {
            AccessAckData(size = bndc.size, source = bndc.source, denied = false.B, addr = bndc.address, data = bndc.data, fwd = true.B)
          }

        } else if (bndc.opcode.litValue() == 2) {

          assert(bndc.param.litValue() == 0, "(C) Non-zero param field for HINTACK TLBundle")
          assert(!bndc.corrupt.litToBoolean, "(C) Corrupt HINTACK TLBundle")
          HintAck(size = bndc.size, source = bndc.source, addr = bndc.address, denied = false.B, fwd = true.B)

        } else if (bndc.opcode.litValue() == 4) {

          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() < 6, s"(B) Non-valid PARAM (${bndc.param}) for PROBEACK Bundle")
          assert(!bndc.corrupt.litToBoolean, "(C) Corrupt PROBEACK TLBundle")
          ProbeAck(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address)

        } else if (bndc.opcode.litValue() == 5) {

          // Assertions checking on first TLBundle
          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() < 6, s"(B) Non-valid PARAM (${bndc.param}) for PROBEACKDATA Bundle")
          assert(alignedLg(bndc.address, bndc.size), s"(B) PROBEACKDATA Address (${bndc.address}) is NOT aligned with size (${bndc.size})")

          if (bndsq.size > 0) {
            var datas = new ListBuffer[UInt]
            datas += bndc.data

            while (bndsq.nonEmpty) {
              // Checking if other bundles in burst are valid
              val other_bndc = bndsq.dequeue().asInstanceOf[TLBundleC]

              assert(bndc.param.litValue() == other_bndc.param.litValue())
              assert(bndc.size.litValue() == other_bndc.size.litValue())
              assert(bndc.address.litValue() == other_bndc.address.litValue())

              datas += other_bndc.data
            }
            ProbeAckDataBurst(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address, datas = datas.toList)
          } else {
            ProbeAckData(param = bndc.param, size = bndc.size, source = bndc.source,addr = bndc.address, data = bndc.data)
          }

        } else if (bndc.opcode.litValue() == 6) {

          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() < 6, s"(B) Non-valid PARAM (${bndc.param}) for RELEASE Bundle")
          assert(!bndc.corrupt.litToBoolean, "(C) Corrupt RELEASE TLBundle")
          Release(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address)

        } else if (bndc.opcode.litValue() == 7) {

          // Assertions checking on first TLBundle
          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() < 6, s"(B) Non-valid PARAM (${bndc.param}) for RELEASEDATA Bundle")
          assert(alignedLg(bndc.address, bndc.size), s"(B) RELEASEDATA Address (${bndc.address}) is NOT aligned with size (${bndc.size})")

          if (bndsq.size > 0) {
            var datas = new ListBuffer[UInt]
            datas += bndc.data

            while (bndsq.nonEmpty) {
              // Checking if other bundles in burst are valid
              val other_bndc = bndsq.dequeue().asInstanceOf[TLBundleC]

              assert(bndc.param.litValue() == other_bndc.param.litValue())
              assert(bndc.size.litValue() == other_bndc.size.litValue())
              assert(bndc.address.litValue() == other_bndc.address.litValue())

              datas += other_bndc.data
            }
            ReleaseDataBurst(param = bndc.param, size = bndc.size, source = bndc.source, addr = bndc.address, datas = datas.toList)
          } else {
            ReleaseData(param = bndc.param, size = bndc.size, source = bndc.source,addr = bndc.address, data = bndc.data)
          }

        } else {

          assert(false, "(C) Invalid OPCODE on C Channel")
          AccessAck(size = 0.U, source = bndc.source, denied = true.B, fwd = true.B)

        }
      case _: TLBundleD =>
        val bndc = bnd.asInstanceOf[TLBundleD]
        if (bndc.opcode.litValue() == 0) {

          assert(bndc.param.litValue() == 0, "(D) Non-zero param field for ACCESSACK TLBundle")
          assert(!bndc.corrupt.litToBoolean, "(D) Corrupt ACCESSACK TLBundle")
          AccessAck(size = bndc.size, source = bndc.source, denied = bndc.denied)

        } else if (bndc.opcode.litValue() == 1) {

          // Assertions checking on first TLBundle
          assert(bndc.param.litValue() == 0, "(D) Non-zero param field for ACCESSACKDATA TLBundle")
          if (bndc.denied.litToBoolean) {
            assert(bndc.corrupt.litToBoolean, "(D) ACCESSACKDATA denied but not corrupt")
          }

          if (bndsq.size > 0) {
            var datas = new ListBuffer[UInt]
            datas += bndc.data

            while (bndsq.nonEmpty) {
              // Checking if other bundles in burst are valid
              val other_bndc = bndsq.dequeue().asInstanceOf[TLBundleD]

              assert(bndc.param.litValue() == other_bndc.param.litValue())
              assert(bndc.size.litValue() == other_bndc.size.litValue(), s"${bndc}, ${other_bndc}")

              datas += other_bndc.data
            }
            AccessAckDataBurst(size = bndc.size, source = bndc.source, denied = bndc.denied, datas = datas.toList)
          } else {
            AccessAckData(size = bndc.size, source = bndc.source, denied = bndc.denied, data = bndc.data)
          }

        } else if (bndc.opcode.litValue() == 2) {

          assert(bndc.param.litValue() == 0, "(D) Non-zero param field for HINTACK TLBundle")
          assert(!bndc.corrupt.litToBoolean, "(D) Corrupt HINTACK TLBundle")
          HintAck(size = bndc.size, source = bndc.source, denied = bndc.denied)

        } else if (bndc.opcode.litValue() == 4) {

          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() < 3, "(D) Non-valid param field for GRANT TLBundle")
          assert(!bndc.corrupt.litToBoolean, "(D) Corrupt GRANT TLBundle")
          Grant(param = bndc.param, size = bndc.size, source = bndc.source, sink = bndc.sink, denied = bndc.denied)

        } else if (bndc.opcode.litValue() == 5) {

          // Assertions checking on first TLBundle
          assert(bndc.param.litValue() >= 0 && bndc.param.litValue() < 3, "(D) Non-valid param field for GRANTDATA TLBundle")
          if (bndc.denied.litToBoolean) {
            assert(bndc.corrupt.litToBoolean, "(D) GRANTDATA denied but not corrupt")
          }

          if (bndsq.size > 0) {
            var datas = new ListBuffer[UInt]
            datas += bndc.data

            while (bndsq.nonEmpty) {
              // Checking if other bundles in burst are valid
              val other_bndc = bndsq.dequeue().asInstanceOf[TLBundleD]

              assert(bndc.param.litValue() == other_bndc.param.litValue())
              assert(bndc.size.litValue() == other_bndc.size.litValue())

              datas += other_bndc.data
            }
            GrantDataBurst(param = bndc.param, size = bndc.size, source = bndc.source, sink = bndc.sink, denied = bndc.denied, datas = datas.toList)
          } else {
            GrantData(param = bndc.param, size = bndc.size, source = bndc.source, sink = bndc.sink, denied = bndc.denied, data = bndc.data)
          }

        } else if (bndc.opcode.litValue() == 6) {

          assert(bndc.param.litValue() == 0, "(D) Non-zer0 param field for RELEASEACK TLBundle")
          assert(!bndc.corrupt.litToBoolean, "(D) Corrupt RELEASEACK TLBundle")
          assert(!bndc.denied.litToBoolean, "(D) RELEASEACK cannot be denied.")

          ReleaseAck(size = bndc.size, source = bndc.source)

        } else {

          assert(false, "(D) Invalid OPCODE on D Channel")
          AccessAck(size = 0.U, source = bndc.source, denied = true.B)

        }
      case _: TLBundleE =>
        val bndc = bnd.asInstanceOf[TLBundleE]
        // No asserts
        GrantAck(sink = bndc.sink)
    }
  }
  */

  // Helper methods for filtering monitored transactions
  def filterA (txn : TLChannel) : Boolean = {
    txn match {
      case _: TLBundleA => true
      case _ => false
    }
  }

  def filterB (txn : TLChannel) : Boolean = {
    txn match {
      case _: TLBundleB => true
      case _ => false
    }
  }

  def filterC (txn : TLChannel) : Boolean = {
    txn match {
      case _: TLBundleC => true
      case _ => false
    }
  }

  def filterD (txn : TLChannel) : Boolean = {
    txn match {
      case _: TLBundleD => true
      case _ => false
    }
  }

  def filterE (txn : TLChannel) : Boolean = {
    txn match {
      case _: TLBundleE => true
      case _ => false
    }
  }

  // Arithmetic Helper Functions
  def max(a : UInt, b : UInt) : UInt = {
    if (a.litValue() < b.litValue()) b else a
  }

  def min(a : UInt, b : UInt) : UInt = {
    if (a.litValue() < b.litValue()) a else b
  }

  def maxu(a : UInt, b : UInt) : UInt = {
    // TODO Fix for unsigned
    if (a.litValue() < b.litValue()) b else a
  }

  def minu(a : UInt, b : UInt) : UInt = {
    // TODO Fix for unsigned
    if (a.litValue() < b.litValue()) a else b
  }

  def add(a : UInt, b : UInt) : UInt = {
    // Will overflow
    (a.litValue() + b.litValue()).U(64.W)
  }

  // Logistic Helper Functions
  def xor(a : UInt, b : UInt) : UInt = {
    (a.litValue() ^ b.litValue()).U(64.W)
  }

  def or(a : UInt, b : UInt) : UInt = {
    (a.litValue() | b.litValue()).U(64.W)
  }

  def and(a : UInt, b : UInt) : UInt = {
    (a.litValue() & b.litValue()).U(64.W)
  }

  // a must be the old value
  def swap(a : UInt, b : UInt) : UInt = {
    a
  }

  def toByteMask(mask : UInt) : BigInt = {
    var maskInt = mask.litValue()
    var result: BigInt = 0
    var i = 0
    do {
      if ((maskInt & 1) == 1) {
        result = result | 0xff << (i * 8)
      }
      maskInt = maskInt >> 1
      i = i + 1
    } while (maskInt > 0 )

    result
  }

  // Repeat Permissions for given size
  def permRepeater(size: UInt, perm: UInt, beatBytes : Int = 8) : List[UInt] = {
    val sizeInt = 1 << size.litValue().toInt
    var tempResult: BigInt = 0
    var resultList = ListBuffer[UInt]()
    var reset = true

    for (i <- 0 until sizeInt) {
      tempResult = (tempResult << 8) | perm.litValue()
      reset = false

      // Separating into separate beats
      if (i % beatBytes == (beatBytes - 1)) {
        resultList += tempResult.U((beatBytes * 8).W)
        tempResult = 0
        reset = true
      }
    }

    // Any dangling data
    if (!reset) {
      resultList += tempResult.U((beatBytes * 8).W)
    }

    resultList.toList
  }

  // State is a byte-addressed HashMap
  def readData(state: HashMap[Int,Int], size: UInt, address: UInt, mask: UInt, beatBytes: Int = 8): List[UInt] = {
    val sizeInt = 1 << size.litValue().toInt
    val addressInt = address.litValue().toInt
    val byteMask = toByteMask(mask)
    var tempResult: BigInt = 0
    var resultList = ListBuffer[UInt]()
    var reset = true

    for (i <- 0 until sizeInt) {
      tempResult = tempResult | (state.getOrElse(addressInt + i, 0) << (i * 8))
      reset = false

      // Separating into separate beats
      if (i % beatBytes == (beatBytes - 1)) {
        resultList += (tempResult & byteMask).U((beatBytes * 8).W)
        tempResult = 0
        reset = true
      }
    }

    // Any dangling data
    if (!reset) {
      resultList += (tempResult & byteMask).U((beatBytes * 8).W)
    }

    resultList.toList
  }

  // State is a byte-addressed HashMap
  def writeData(state : HashMap[Int,Int], size: UInt, address: UInt, datas: List[UInt], masks: List[UInt], beatBytes: Int = 8): Unit = {
    val sizeInt = 1 << size.litValue().toInt
    val addressInt = address.litValue().toInt
    var allData: BigInt = 0
    var allMask: BigInt = 0

    assert(datas.length == masks.length, "Data and Mask lists are not of equal lengths.")

    // Condensing all data and masks
    for (i <- 0 until datas.length) {
      allData = allData | (datas(i).litValue() << (beatBytes * 8 * i))
      allMask = allMask | (toByteMask(masks(i)) << (beatBytes * 8 * i))
    }

    // Writing all data with masks
    val byteMask = (1 << 8) - 1
    var preMaskData = 0
    var dataMask = 0
    for (i <- 0 until sizeInt) {
      // Getting a byte worth of data
      preMaskData = (allData & byteMask).toInt
      dataMask = (allMask & byteMask).toInt

      // Updating HashMap
      state(addressInt + i) = (state.getOrElse(addressInt + i, 0) & ~dataMask) | (preMaskData & dataMask)

      // Shifting to next byte
      allData  = allData >> 8
      allMask = allMask >> 8
    }
  }

  // Response function required for TL SDrivers
  // TODO: this is written poorly
  def testResponse (input: List[TLChannel], state: HashMap[Int,Int])(implicit p: TLBundleParameters) : (Seq[TLChannel], HashMap[Int,Int]) = {
    val beatBytesSize = 3
    // Default response is corrupt transaction
    var responseTLTxn = Seq(AccessAck(denied=0, size=0, source=0))
    // Making internal copy of state (non-destructive)
    val state_int = state.clone()

    // Assert
    assert(input.nonEmpty, "ERROR: List of TLBundles is EMPTY for testResponse slaving function.")


    val inputhead = input.head
    // Transaction response
    inputhead match {
      case txnc: TLBundleA =>
        txnc.opcode.litValue().toInt match {
          case TLOpcodes.Get =>
            // Assert
            assert(input.isEmpty, "ERROR: Get request has too many beats.")

            // Read Data
            val readOut = readData(state = state_int, size = txnc.size, address  = txnc.address, mask = txnc.mask)

            if (txnc.size.litValue() > beatBytesSize)
              responseTLTxn = AccessAckDataBurst(source = txnc.source.litValue().toInt, denied = 0, data = readOut.map(_.litValue()))
            else
              responseTLTxn = Seq(AccessAckData(readOut.head.litValue(), 0, txnc.source.litValue().toInt))

          case TLOpcodes.PutFullData | TLOpcodes.PutPartialData =>
            val size = if (txnc.size.litValue().toInt > beatBytesSize) beatBytesSize.U else txnc.size
            val source = txnc.source
            val address = txnc.address
            val datas = input.map(_.asInstanceOf[TLBundleA].data)
            val masks = input.map(_.asInstanceOf[TLBundleA].mask)


            // Write Data
            writeData(state = state_int, size = size, address = address, datas = datas, masks = masks)

            // Response
            responseTLTxn = Seq(AccessAck(size = size.litValue().toInt, source = source.litValue().toInt, denied = 0))

          case TLOpcodes.ArithmeticData =>
            val size = if (txnc.size.litValue().toInt > beatBytesSize) beatBytesSize.U else txnc.size
            val param = txnc.param
            val source = txnc.source
            val address = txnc.address
            val datas = input.map(_.asInstanceOf[TLBundleA].data)
            val masks = input.map(_.asInstanceOf[TLBundleA].mask)

            var function : (UInt, UInt) => UInt = min
            param.litValue().toInt match {
              case 0 => function = min
              case 1 => function = max
              case 2 => function = minu
              case 3 => function = maxu
              case 4 => function = add
            }

            // Reading Old Data (to return)
            val oldData = readData(state = state_int, size = size, address = address, mask = 0xff.U)

            // Creating newData (to write)
            var newData = ListBuffer[UInt]()
            for (((n, m), o) <- (datas zip masks) zip oldData) {
              newData += function(o, (n.litValue() & toByteMask(m)).U)
            }

            writeData(state = state_int, size = txnc.size, address = txnc.address, datas = newData.toList, masks = List.fill(newData.length)(0xff.U))
            responseTLTxn = AccessAckDataBurst(source = source.litValue().toInt, denied = 0, data = oldData.map(_.litValue()))

          case TLOpcodes.LogicalData =>
            val size = if (txnc.size.litValue().toInt > beatBytesSize) beatBytesSize.U else txnc.size
            val param = txnc.param
            val source = txnc.source
            val address = txnc.address
            val datas = input.map(_.asInstanceOf[TLBundleA].data)
            val masks = input.map(_.asInstanceOf[TLBundleA].mask)

            var function : (UInt, UInt) => UInt = min
            txnc.param.litValue().toInt match {
              case 0 => function = xor
              case 1 => function = or
              case 2 => function = and
              case 3 => function = swap
            }

            // Reading Old Data (to return)
            val oldData = readData(state = state_int, size = txnc.size, address = txnc.address, mask = 0xff.U)

            // Creating newData (to write)
            var newData = ListBuffer[UInt]()
            for (((n, m), o) <- (datas zip masks) zip oldData) {
              newData += function(o, (n.litValue() & toByteMask(m)).U)
            }

            writeData(state = state_int, size = txnc.size, address = txnc.address, datas = newData.toList, masks = List.fill(newData.length)(0xff.U))
            responseTLTxn = AccessAckDataBurst(source = source.litValue().toInt, denied = 0, data = oldData.map(_.litValue()))

          case 5 => // TODO: TLOpcodes doesn't contain Intent (opcode = 5)
            // Currently don't accept hints
            responseTLTxn = Seq(HintAck(size = txnc.size.litValue().toInt, source = txnc.source.litValue().toInt, denied = 1))
        }
    }
    (responseTLTxn, state_int)
  }
}