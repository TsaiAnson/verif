package verif

import freechips.rocketchip.tilelink.{TLBundleA, TLBundleB, TLBundleC, TLBundleD, TLBundleE, TLBundleParameters, TLChannel}
import verif.TLUtils._
import verif.TLTransaction._
import chisel3._
import chisel3.util.log2Ceil

import scala.collection.mutable.{HashMap, ListBuffer, Queue}

// TL Transaction Fuzzer that follows TL-C protocol
// Note: Currently only supports one acquire and release in-flight at a given time. Can be extended.
class TLCFuzzer(params: TLBundleParameters, txnGen: Option[TLTransactionGenerator], forceTxn: Seq[TLTransaction] = Seq(), cacheBlockSize: Int) {
  implicit val p = params

  // Internal Structures TODO Hardcoded BlockSize
  val permState = new RWPermState(cacheBlockSize)
  val dataState = HashMap[Int, Int]()

  // Used for state processing (check if permissions/data were given etc)
  val bOutput = ListBuffer[TLChannel]()
  val dOutput = ListBuffer[TLChannel]()

  // Used for acquire addressing (!!! supports only one acquire in-flight) TODO use source as IDs
  var acquireInFlight = false
  var acquireAddr = 0
  // Used for release addressing (!!! supports only one release in-flight) TODO use source as IDs
  var releaseInFlight = false
  // Temporary for non-concurrent operations, will remove constraint later
  var inFlight = false
  // Keep track of which txns have been forced (manually generated)
  var manTxn = ListBuffer[TLChannel]() ++ forceTxn
  // Received transactions to be processed
  val tlProcess = ListBuffer[TLChannel]()
  // TLBundles to be pushed
  val queuedTLBundles = ListBuffer[TLChannel]()
  // Map param => permission for Acquire and Release
  val acquirePermMap = Map[Int, Int](0 -> 1, 1 -> 2, 2 -> 2)
  val releasePermMap = Map[Int, Int](0 -> 1, 1 -> 0, 2 -> 0, 3 -> 2, 4 -> 1, 5 -> 0)

  def addTxns(add: Seq[TLChannel]): Unit = {
    manTxn ++= add
  }

  def next(resp: Seq[TLChannel]): Seq[TLChannel] = {
    val txFromSlave = resp.flatMap {
      case _: TLBundleA | _: TLBundleC | _: TLBundleE => None
      case other => Some(other)
    }
    bOutput ++= txFromSlave.collect { case t: TLBundleB => t }
    dOutput ++= txFromSlave.collect { case t: TLBundleD => t }

    // Get Channel D and B txns
    val dComplete = getNextCompleteTLTxn(dOutput)
    val bComplete = getNextCompleteTLTxn(bOutput)
    // D first as Release -> Probe dependency
    if (dComplete.isDefined) {
      dOutput.remove(0, dComplete.get.size)
      tlProcess ++= dComplete.get.filter({ x: TLChannel =>
        val opCode = x.asInstanceOf[TLBundleD].opcode.litValue().toInt
        opCode == TLOpcodes.AccessAck || opCode == TLOpcodes.AccessAckData || opCode == TLOpcodes.Grant ||
          opCode == TLOpcodes.GrantData || opCode == TLOpcodes.ReleaseAck
      })
    }
    if (bComplete.isDefined) {
      bOutput.remove(0, bComplete.get.size)
      tlProcess ++= bComplete.get.filter({ x: TLChannel =>
        val opCode = x.asInstanceOf[TLBundleB].opcode.litValue().toInt
        opCode == TLOpcodes.ProbePerm || opCode == TLOpcodes.ProbeBlock
      })
    }

    // Process Input Transactions
    var processIndex = 0
    while (processIndex < tlProcess.length) {
      val txn = tlProcess(processIndex)
      txn match {
        case txnc: TLBundleB => // Always either ProbeBlock or ProbePerm
          if (!releaseInFlight) {
            // Calculating permissions
            val oldPerm = permState.getPerm(txnc.address.litValue().toInt)
            val newPerm = 2 - txnc.param.litValue().toInt
            var newParam = 0

            // If permissions are the same
            if (newPerm == oldPerm) {
              // Report param
              newParam = 5 - oldPerm
            } else {
              // Shrink Param
              if (oldPerm == 2) {
                newParam = if (newPerm == 1) 0 else 1
              } else {
                newParam = 2
              }

              // Writing permissions
              permState.setPerm(txnc.address.litValue().toInt, newPerm)
            }

            // Remove before return
            tlProcess.remove(processIndex)
            if (txnc.opcode.litValue() == TLOpcodes.ProbePerm) {
              // For ProbePerm, no ProbeAcKData needed
              return Seq(ProbeAck(TLPermission.PruneOrReport.fromInt(newParam), txnc.address.litValue(), txnc.size.litValue().toInt,
                source = txnc.source.litValue().toInt))
            } else if (txnc.opcode.litValue() == TLOpcodes.ProbeBlock) {
              // If old permission included write access, need to send back dirty data
              if (oldPerm == 2 && newPerm != oldPerm) {
                return ProbeAckDataBurst(TLPermission.PruneOrReport.fromInt(newParam), txnc.address.litValue(),
                  readData(dataState, size = txnc.size, address = txnc.address, mask = 0xff.U).map(_.litValue()),
                  source = txnc.source.litValue().toInt)
              } else {
                return Seq(ProbeAck(TLPermission.PruneOrReport.fromInt(newParam), txnc.address.litValue(), txnc.size.litValue().toInt,
                  source = txnc.source.litValue().toInt))
              }
            }
          } else {
            // ProbePerm/Block are always one TLBundle
            processIndex += 1
          }

        case txnc: TLBundleD => // Always either Grant, GrantData, ReleaseAck, AccessAck, or AccessAckData
          if (txnc.opcode.litValue() == TLOpcodes.Grant || txnc.opcode.litValue() == TLOpcodes.GrantData) {
            if (!txnc.denied.litToBoolean) {

              // Writing Permissions
              val newPerm = (2 - txnc.param.litValue().toInt)
              permState.setPerm(acquireAddr, newPerm)

              // Writing Data
              if (txnc.opcode.litValue() == TLOpcodes.GrantData) {
                val beats = 1 << math.max(txnc.size.litValue().toInt - log2Ceil(params.dataBits/8), 0)
                writeData(state = dataState, size = txnc.size, address = acquireAddr.U,
                  datas = tlProcess.dropRight(tlProcess.length - beats).map {
                    _.asInstanceOf[TLBundleD].data
                  },
                  masks = List.fill(beats)(0xff.U))
              }

              acquireInFlight = false
              inFlight = false

              if (txnc.opcode.litValue() == TLOpcodes.GrantData) {
                val beats = 1 << math.max(txnc.size.litValue().toInt - log2Ceil(params.dataBits/8), 0)
                tlProcess.remove(processIndex, beats)
              } else {
                tlProcess.remove(processIndex)
              }

              return Seq(GrantAck(sink = txnc.sink.litValue().toInt))
            } else {
              if (txnc.opcode.litValue() == TLOpcodes.GrantData) {
                val beats = 1 << math.max(txnc.size.litValue().toInt - log2Ceil(params.dataBits/8), 0)
                tlProcess.remove(processIndex, beats)
              } else {
                tlProcess.remove(processIndex)
              }
            }

          } else if (txnc.opcode.litValue() == TLOpcodes.ReleaseAck) {
            tlProcess.remove(processIndex)

            // Now able to queue up more releases
            releaseInFlight = false
            inFlight = false
          } else {
            // AccessAck and AccessAckData
            if (txnc.opcode.litValue() == TLOpcodes.AccessAckData) {
              val beats = 1 << math.max(txnc.size.litValue().toInt - log2Ceil(params.dataBits/8), 0)
              tlProcess.remove(processIndex, beats)
            } else {
              tlProcess.remove(processIndex)
            }
            inFlight = false
          }
      }
    }

    // If nothing queued, check if there is manually generated txns to push
    if (manTxn.nonEmpty && queuedTLBundles.isEmpty) {
      val complete = getNextCompleteTLTxn(manTxn)
      if (complete.isDefined) {
        queuedTLBundles ++= complete.get
        manTxn.remove(0, complete.get.size)
      }
    }

    // Only generate transactions if nothing to send (TODO: change when concurrent)
    if (queuedTLBundles.isEmpty && txnGen.isDefined) {
      // Generate next transaction (passing in permissions)
      queuedTLBundles ++= txnGen.get.generateTransactions(1, permState)
    }

    // Determining next transaction to be pushed
    // Currently limit inFlight instructions, will expand soon
    var inputIndex = 0
    while (!inFlight && inputIndex < queuedTLBundles.length) {

      // Extra processessing for AcquireBlock/Perm (A), Release/Data (C), GrantAck (E)
      val txnHead = queuedTLBundles(inputIndex)
      txnHead match {
        case txnc: TLBundleA =>
          if (txnc.opcode.litValue().toInt == TLOpcodes.AcquireBlock || txnc.opcode.litValue().toInt == TLOpcodes.AcquirePerm) {
            if (acquireInFlight || releaseInFlight) {
              inputIndex += 1
            } else {
              acquireAddr = txnc.address.litValue().toInt
              acquireInFlight = true
              inFlight = true
              return Seq(queuedTLBundles.remove(inputIndex))
            }
          } else {
            val beats = if (isNonBurst(txnHead)) 1 else 1 << math.max(txnc.size.litValue().toInt - log2Ceil(params.dataBits/8), 0)
            val results = queuedTLBundles.dropRight(queuedTLBundles.length - beats)
            queuedTLBundles.remove(inputIndex, beats)
            inFlight = true

            return results
          }


        case txnc: TLBundleC =>
          // Warning: May be invalid release if manually generated. TLTransactionGenerator should not generate invalid txns
          if (txnc.opcode.litValue().toInt == TLOpcodes.Release || txnc.opcode.litValue().toInt == TLOpcodes.ReleaseData) {
            val beats = if (isNonBurst(txnHead)) 1 else 1 << math.max(txnc.size.litValue().toInt - log2Ceil(params.dataBits/8), 0)
            if (acquireInFlight || releaseInFlight) {
              inputIndex += beats
            } else {
              val result = queuedTLBundles.dropRight(queuedTLBundles.length - beats)
              queuedTLBundles.remove(inputIndex, beats)
              releaseInFlight = true
              inFlight = true

              // Shrinking permissions
              permState.setPerm(txnc.address.litValue().toInt, releasePermMap(txnc.param.litValue().toInt))

              return result
            }
          } else {
            val beats = if (isNonBurst(txnHead)) 1 else 1 << math.max(txnc.size.litValue().toInt - log2Ceil(params.dataBits/8), 0)
            val result = queuedTLBundles.dropRight(queuedTLBundles.length - beats)
            queuedTLBundles.remove(inputIndex, beats)
            inFlight = true

            return result
          }

        case _: TLBundleE =>
          return Seq(queuedTLBundles.remove(inputIndex))
      }
    }
    return Seq()
  }
}
