package verif

import chisel3.util.log2Ceil
import freechips.rocketchip.tilelink.{TLBundleA, TLBundleD, TLBundleParameters, TLChannel}
import TLUtils._
import TLTransaction._

import scala.collection.mutable

// TL-U (L/H) Transaction dispatcher that is compliant with TL protocols
class TLUDispatcher(params: TLBundleParameters, txnGen: Option[TLTransactionGenerator], forceTxn: Seq[TLChannel] = Seq()) {
  implicit val p = params

  // Internal state mapping source -> state. States: 0 (Idle), 1 (pending AccessAck), 2 (pending HintAck)
  val sourceState = new mutable.HashMap[Int,Int]()
  val beatSize = log2Ceil(params.dataBits / 8)

  // Queued transactions
  var queuedTxns = mutable.ListBuffer[TLChannel]() ++ forceTxn
  // Incomplete Responses
  val respD: mutable.ListBuffer[TLChannel] = mutable.ListBuffer()

  def addTxns(add: Seq[TLChannel]): Unit = {
    queuedTxns ++= add
  }

  def next(resp: Seq[TLChannel]): Seq[TLChannel] = {
    // Processing response transactions
    respD ++= resp.collect { case t: TLBundleD => t }
    var procResp = true
    while (procResp) {
      procResp = false
      val txn = getNextCompleteTLTxn(respD).getOrElse(Seq())
      if (txn.nonEmpty) {
        respD.remove(0, txn.size)
        val head = txn.head.asInstanceOf[TLBundleD]
        val source = head.source.litValue().toInt
        if (head.opcode.litValue().toInt == TLOpcodes.AccessAck || head.opcode.litValue().toInt == TLOpcodes.AccessAckData) {
          if (sourceState.getOrElse(source, 0) != 1) println(s"ERROR: Unexpected AccessAck/Data response: $head")
          sourceState(source) = 0
        } else if (head.opcode.litValue().toInt == TLOpcodes.HintAck) {
          if (sourceState.getOrElse(source, 0) != 2) println(s"ERROR: Unexpected HintAck response: $head")
          sourceState(source) = 0
        } else {
          println(s"ERROR: Unknown TL-U response: $head")
        }
        procResp = true
      }
    }

    // Generating next transactions
    if (queuedTxns.isEmpty && txnGen.isDefined) {
      queuedTxns ++= txnGen.get.generateTransactions(1)
    }

    // Check legality of transaction
    var dispatch = getNextCompleteTLTxn(queuedTxns).getOrElse(Seq())
    if (dispatch.nonEmpty) {
      val head = dispatch.head.asInstanceOf[TLBundleA]
      val source = head.source.litValue().toInt
      if (sourceState.getOrElse(source, 0) == 0) {
        // Remove from queue if taken from queue
        if (queuedTxns.nonEmpty) queuedTxns.remove(0, dispatch.size)
        // Updating state
        val op = head.opcode.litValue().toInt
        if (op <= TLOpcodes.Get) sourceState(source) = 1
        else if (op == TLOpcodes.Hint) sourceState(source) = 2
        else println(s"ERROR: Unknown TL-U request: $head")
      } else {
        // Don't send
        dispatch = Seq()
      }
    }

    dispatch
  }
}