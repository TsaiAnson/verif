package verif

import chisel3._
import freechips.rocketchip.tilelink.{TLBundleA, TLBundleD, TLBundleParameters, TLChannel}
import verif.TLTransaction.TLOpcodes

import scala.collection.mutable.{HashMap, Queue}

trait PSLMemoryModel[T,M] {
  def model(input: Seq[T]): Seq[Option[PSLMemoryState[M]]]
}

class PSLTLMemoryModel(p: TLBundleParameters) extends PSLMemoryModel[TLChannel, UInt] {
  val mem_model = new TLMemoryModel(p)
  var mem_model_state = TLMemoryModel.State.empty()
  // Maps source to remaining burst
  val dataBuffer = new HashMap[Int, Queue[UInt]]()

  // Uses the optimized memory state for TL
  def model(input: Seq[TLChannel]): Seq[Option[PSLMemoryState[UInt]]] = {
    val (responseTxns, newState) = mem_model.respondFromState(input.collect{case t: TLBundleA => t}, mem_model_state)
    mem_model_state = newState
    for (resp <- responseTxns) {
      resp match {
        case txnd: TLBundleD =>
          if (txnd.opcode.litValue() == TLOpcodes.AccessAckData) {
            if (!dataBuffer.contains(txnd.source.litValue().toInt)) {
              dataBuffer(txnd.source.litValue().toInt) = new Queue[UInt]()
            }
            dataBuffer(txnd.source.litValue().toInt).enqueue(txnd.data)
          }
        case _ => // Do nothing
      }
    }

    var result = Seq[Option[PSLMemoryState[UInt]]]()
    for (txn <- input) {
      txn match {
        case txnd: TLBundleD =>
          if (txnd.opcode.litValue() == TLOpcodes.AccessAckData) {
            result = result :+ Some(new PSLOptTLMemoryState(dataBuffer(txnd.source.litValue().toInt).dequeue()))
          } else {
            result = result :+ None
          }
        case _ => result = result :+ None
      }
    }

    result
  }
}
