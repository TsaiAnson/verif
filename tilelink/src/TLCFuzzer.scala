import freechips.rocketchip.tilelink.{TLBundleA, TLBundleB, TLBundleC, TLBundleD, TLBundleE, TLBundleParameters, TLChannel}
import verif.TLUtils._
import verif.TLTransaction._
import chisel3._
import verif.{TLDriverMaster, TLMonitor, TLTransactionGenerator}

import scala.collection.mutable.{HashMap, ListBuffer, Queue}

// TL Transaction generator that follows TL-C protocol
// Requires a TLDriver, TLMonitor, TLTransactionGenerator (for Generator params)
class TLCFuzzer(driver: TLDriverMaster, monitor: TLMonitor, params: TLBundleParameters, forceTxn: Seq[TLChannel] = Seq(),
                txnGen: TLTransactionGenerator) {
  implicit val p = params

  // Internal Structures
  val permState = new RWPermState()
  // May remove dataState completely
  val dataState = HashMap[Int,Int]()

  // Used for state processing (check if permissions/data were given etc)
  val bOutput = ListBuffer[TLChannel]()
  val dOutput = ListBuffer[TLChannel]()

  // Used for acquire addressing (!!! supports only one acquire in-flight) TODO use source as IDs
  var acquireInFlight = false
  var acquireAddr = 0.U
  // Used for release addressing (!!! supports only one release in-flight) TODO use source as IDs
  var releaseInFlight = false
  // Temporary for non-concurrent operations
  var inFlight = false
  // Received transactions to be processed
  val tlProcess = ListBuffer[TLChannel]()
  // TLBundles to be pushed
  val queuedTLBundles = Queue[TLChannel]()
  // Map param => permission for Acquire and Release
  val acquirePermMap = Map[Int,Int](0 -> 1, 1 -> 2, 2 -> 2)
  val releasePermMap = Map[Int,Int](0 -> 1, 1 -> 0, 2 -> 0, 3 -> 2, 4 -> 1, 5 -> 0)

  def fuzzTxn(count: Int): Unit = {
    while (count > 0) {

      // Generate next transaction (passing in permissions)
      val txns = txnGen.generateTransactions(1, permState)

      val txnHead = txns.head
      txnHead match {
        case _: TLBundleA =>
          // Integrate functionality from process method

        case _: TLBundleC =>
          // ...

        case _: TLBundleE =>
          // ...
      }

      driver.push(txns)
    }
  }


  // After receiving txns (on channel B and D), return any TL responses (on A, C, or E)
  // Whole method has to be re-written for raw TLBundles
  def process(txns: Seq[TLChannel]): Seq[TLChannel] = {
    bOutput ++= txns.collect { case t: TLBundleB => t}
    dOutput ++= txns.collect { case t: TLBundleD => t}

    // Process output transactions
    if (bOutput.nonEmpty || dOutput.nonEmpty) {
      if (isCompleteTLTxn(bOutput.toList)) {
        tlProcess ++= bOutput
        bOutput.clear()
      } else if (isCompleteTLTxn(dOutput.toList)) {
        tlProcess ++= dOutput
        dOutput.clear()
      }
    }
    queuedTLBundles.clear()

    var processIndex = 0
    while (processIndex < tlProcess.length) {
      val tlTxn = tlProcess(processIndex)
      tlTxn match {
        case txnc: TLBundleD => {
          txnc.opcode.litValue().toInt match {
            case TLOpcodes.Grant =>
              if (!txnc.denied.litToBoolean) {
                // Writing permissions
                val newPerm = (2 - txnc.param.litValue().toInt).U
                val permData = permRepeater(size = txnc.size, perm = newPerm)
//                writeData(state = permState, size = txnc.size, address = acquireAddr, datas = permData,
//                  masks = List.fill(permData.length)(0xff.U))

                queuedTLBundles += GrantAck(sink = txnc.sink.litValue().toInt)
                tlProcess.remove(processIndex)

                // Now able to queue up more releases
                acquireInFlight = false
                inFlight = false
              }
            case TLOpcodes.GrantData =>
              if (!txnc.denied.litToBoolean) {
                // Writing permissions and data
                val newPerm = (2 - txnc.param.litValue().toInt).U
                val permData = permRepeater(size = txnc.size, perm = newPerm)
//                writeData(state = permState, size = txnc.size, address = acquireAddr, datas = permData,
//                  masks = List.fill(permData.length)(0xff.U))
                writeData(state = dataState, size = txnc.size, address = acquireAddr, datas = List(txnc.data), masks = List(0xff.U))

                queuedTLBundles += GrantAck(sink = txnc.sink.litValue().toInt)
                tlProcess.remove(processIndex)

                // Now able to queue up more releases
                acquireInFlight = false
                inFlight = false
              }
            case TLOpcodes.ReleaseAck =>
              tlProcess.remove(processIndex)

              // Now able to queue up more releases
              releaseInFlight = false
              inFlight = false
          }
        }
        case txnc: TLBundleB =>
          txnc.opcode.litValue().toInt match {
            case TLOpcodes.ProbePerm =>
              // Probe (Return ProbeAck) Don't process if pending Release Ack
              if (!releaseInFlight) {
                val oldPerm = 0
                // Given permission
                val newPerm = 2 - txnc.param.litValue().toInt
                var newParam = 0.U

                // If permissions are the same
                if (newPerm == oldPerm) {
                  // Report param
                  newParam = (5 - oldPerm).U
                } else {
                  // Shrink Param
                  if (oldPerm == 2) {
                    newParam = if (newPerm == 1) 0.U else 1.U
                  } else {
                    newParam = 2.U
                  }

                  // Writing permissions
                  val permData = permRepeater(size = txnc.size, perm = newPerm.U)
//                  writeData(state = permState, size = txnc.size, address = txnc.address, datas = permData,
//                    masks = List.fill(permData.length)(0xff.U))
                }

                // TODO: add bundle constructors for
                queuedTLBundles += ProbeAck(param = newParam.litValue().toInt, size = txnc.size.litValue().toInt, source = txnc.source.litValue().toInt
                  , addr = txnc.address.litValue())
                tlProcess.remove(processIndex)
              } else {
                processIndex += 1
              }
            case TLOpcodes.ProbeBlock =>// Probe (Return ProbeAck or ProbeAckData based off perms) Don't process if pending Release Ack
              if (!releaseInFlight) {
                val oldPerm = 0
                // Given permission
                val newPerm = 2 - txnc.param.litValue().toInt
                var newParam = 0.U

                // If permissions are the same
                if (newPerm == oldPerm) {
                  // Report param
                  newParam = (5 - oldPerm).U
                } else {
                  // Shrink Param
                  if (oldPerm == 2) {
                    newParam = if (newPerm == 1) 0.U else 1.U
                  } else {
                    newParam = 2.U
                  }

                  // Writing permissions
                  val permData = permRepeater(size = txnc.size, perm = newPerm.U)
//                  writeData(state = permState, size = txnc.size, address = txnc.address, datas = permData,
//                    masks = List.fill(permData.length)(0xff.U))
                }

                // If old permission included write access, need to send back dirty data
                if (oldPerm == 2 && newPerm != oldPerm) {
                  queuedTLBundles ++= ProbeAckDataBurst(param = newParam.litValue().toInt, source = txnc.source.litValue().toInt, addr = txnc.address.litValue(),
                    data = readData(dataState, size = txnc.size, address = txnc.address, mask = 0xff.U).map(_.litValue()))
                } else {
                  queuedTLBundles += ProbeAck(param = newParam.litValue().toInt, size = txnc.size.litValue().toInt, source = txnc.source.litValue().toInt, addr = txnc.address.litValue())
                }
                tlProcess.remove(processIndex)
              } else {
                processIndex += 1
              }
            // TODO: opcode 5 = Intent, not defined in TLMessages
            case TLOpcodes.Get | TLOpcodes.PutFullData | TLOpcodes.PutPartialData | TLOpcodes.ArithmeticData | TLOpcodes.LogicalData | TLOpcodes.Hint =>
              // Using the testResponse slave function
              // TODO FIX
//              val results = testResponse(input = tlTxn, state = dataState)

//              queuedTLBundles ++= results._1
//              tlProcess.remove(processIndex)
              inFlight = false
          }
        case _ =>
          // Response that requires no processing
          tlProcess.remove(processIndex)
          inFlight = false
      }
      processIndex = processIndex + 1
    }
    queuedTLBundles

    // Determine input transactions
    // Currently limit inFlight instructions, as unsure on handling overloading L2 TODO update
    /*
    var inputIndex = 0
    while (!inFlight && inputIndex < inputTransactions.length) {
      val tlTxn = inputTransactions(inputIndex)

      tlTxn match {
        case _: AcquirePerm | _: AcquireBlock => // Don't issue if pending Grant or Release Ack
          if (acquireInFlight || releaseInFlight) {
            inputIndex += 1
          } else {
            var newParamInt = 0
            tlTxn match {
              case _: AcquirePerm =>
                newParamInt = tlTxn.asInstanceOf[AcquirePerm].param.litValue().toInt
                acquireAddr = tlTxn.asInstanceOf[AcquirePerm].addr
              case _: AcquireBlock =>
                newParamInt = tlTxn.asInstanceOf[AcquireBlock].param.litValue().toInt
                acquireAddr = tlTxn.asInstanceOf[AcquireBlock].addr
            }

            // Sanity check if permission still applies (as of right now the transaction generator statically defines
            // transactions, has no information on the true state of permissions)
            var invalid = false
            val acqAddrInt = acquireAddr.litValue().toInt

            if (newParamInt < 2 && (permState.contains(acqAddrInt) && permState(acqAddrInt) != 0)) {
              invalid = true
            } else if (newParamInt == 2 && ((permState.contains(acqAddrInt) && permState(acqAddrInt) != 1) || !permState.contains(acqAddrInt))) {
              invalid = true
            }

            if (invalid && !allowInvalidTxn) {
              println(s"WARNING: Transaction $tlTxn has invalid parameters (Acquire param doesn't match current state) " +
                s"- SKIPPING. To turn this off, set allowInvalidTxn = true.")
            } else {
              if (invalid) {
                println(s"WARNING: Invalid transaction ($tlTxn) detected, but allowInvalidTxn was set to true.")
              }

              queuedTLBundles ++= TLTransactiontoTLBundles(tlTxn)

              acquireInFlight = true
              inFlight = true
            }

            inputTransactions.remove(inputIndex)
          }
        case _: Release | _: ReleaseData | _: ReleaseDataBurst => // Don't issue if pending Grant or Release Ack
          if (acquireInFlight || releaseInFlight) {
            inputIndex += 1
          } else {
            var releaseAddr = 0.U
            var releaseSize = 0.U
            var releaseParam = 0.U
            tlTxn match {
              case _: Release =>
                val txnc = tlTxn.asInstanceOf[Release]
                releaseAddr = txnc.addr
                releaseSize = txnc.size
                releaseParam = txnc.param
              case _: ReleaseData =>
                val txnc = tlTxn.asInstanceOf[ReleaseData]
                releaseAddr = txnc.addr
                releaseSize = txnc.size
                releaseParam = txnc.param
              case _: ReleaseDataBurst =>
                val txnc = tlTxn.asInstanceOf[ReleaseDataBurst]
                releaseAddr = txnc.addr
                releaseSize = txnc.size
                releaseParam = txnc.param
            }

            // Sanity check if permission still applies (as of right now the transaction generator statically defines
            // transactions, has no information on the true state of permissions)
            var invalid = false
            val relAddrInt = releaseAddr.litValue().toInt
            val relParamInt = releaseParam.litValue().toInt

            if (relParamInt < 2 && ((permState.contains(relAddrInt) && permState(relAddrInt) != 2) || !permState.contains(relAddrInt))) {
              invalid = true
            } else if (relParamInt == 2 && ((permState.contains(relAddrInt) && permState(relAddrInt) != 1) || !permState.contains(relAddrInt))) {
              invalid = true
            }

            // Fixing invalid transactions if enabled -- TODO Not fully tested, temporary
            var fixed = false
            var skipped = false
            var convertToRelease = false
            var fixedParam = releaseParam
            if (invalid && !allowInvalidTxn && fixInvalidTxn) {
              if (relParamInt > 0 && (permState.getOrElse(relAddrInt, 0) > 0)) {
                if (permState.getOrElse(relAddrInt, 0) == 2) {
                  fixedParam = 1.U
                  fixed = true
                } else {
                  convertToRelease = true
                  fixedParam = 2.U
                  fixed = true
                }

                println(s"NOTE: Transaction $tlTxn has invalid parameters (Release param doesn't match current state) " +
                  s" and the initial permission was FIXED to match the current permissions (requested permission stays the same): " +
                  s"($releaseParam -> $fixedParam) To disable automatic fixing, set fixInvalidTxn = false.")

              } else if ((relParamInt > 0 && permState.getOrElse(relAddrInt, 0) == 0) ||
                (relParamInt == 0 && permState.getOrElse(relAddrInt, 0) == 1)) {
                println(s"NOTE: Transaction $tlTxn has invalid parameters (Release param doesn't match current state) " +
                  s" and was SKIPPED since the current permissions already reflect the requested permissions.")

                skipped = true
              } else {
                println(s"WARNING: Transaction $tlTxn has invalid parameters (Release param doesn't match current state) " +
                  s", and was SKIPPED due to requested permission greater than current permissions (unable to fix). To " +
                  s"disable this, set fixInvalidTxn = false and allowInvalidTxn = true.")

                skipped = true
              }
            } else if (invalid && !allowInvalidTxn) {
              println(s"WARNING: Transaction $tlTxn has invalid parameters (Release param doesn't match current state) " +
                s"- SKIPPING. To turn this off, set allowInvalidTxn = true. To enable automatic fixing, set " +
                s"fixInvalidTxn = true.")

              skipped = true
            }

            if (!skipped) {
              if (invalid && !fixInvalidTxn) {
                println(s"WARNING: Invalid transaction ($tlTxn) detected, but allowInvalidTxn was set to true.")
              }

              // Only modifying perms since transaction has data already
              val permData = permRepeater(size = releaseSize, perm = releasePermMap(fixedParam.litValue().toInt).U)
              writeData(state = permState, size = releaseSize, address = releaseAddr, datas = permData,
                masks = List.fill(permData.length)(0xff.U))

              // Fixing up transaction (if enabled)
              var newtlTxn = tlTxn
              if (convertToRelease && fixed) {
                // Convert ReleaseDataBurst to Release
                val temp = tlTxn.asInstanceOf[ReleaseDataBurst]
                newtlTxn = Release(param = fixedParam, size = temp.size, source = temp.source, addr = temp.addr)
              } else if (!convertToRelease && fixed) {
                // Convert Release to ReleaseDataBurst (can only supply 0...)
                val temp = tlTxn.asInstanceOf[Release]
                newtlTxn = ReleaseDataBurst(param = fixedParam, size = temp.size, source = temp.source, addr = temp.addr,
                  datas = List.fill(4)(0.U))
              }

              queuedTLBundles ++= TLTransactiontoTLBundles(newtlTxn)

              releaseInFlight = true
              inFlight = true
            }

            inputTransactions.remove(inputIndex)
          }
        case _ =>
          // TODO Maybe add check if we already have data that is being requested (eg It doesn't make sense to Get an address that we have cached already)
          // TODO But This may also check the forwarding functionality?
          queuedTLBundles ++= TLTransactiontoTLBundles(tlTxn)
          inputTransactions.remove(inputIndex)
          inFlight = true
      }
    }
     */
  }
}
