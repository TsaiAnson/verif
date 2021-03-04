package verif

import chisel3._
import freechips.rocketchip.tilelink.{TLBundleA, TLBundleD, TLChannel, TLBundleParameters}
import scala.collection.mutable.{HashMap, Queue}
import TLTransaction.TLOpcodes

// Custom optimized TL memory state (only one address-data pair per transaction)
class SLOptTLMemoryState(init: UInt = 0.U) extends SLMemoryState[UInt] {
  var int_state = init

  // Addr unused
  def get(addr: Int): UInt = {
    int_state
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case t: SLOptTLMemoryState => this.int_state.litValue() == t.int_state.litValue()
      case _ => false
    }
  }
  override def toString: String = s"${this.getClass.getTypeName}: $int_state"
}

class TLSLMemoryModel(p: TLBundleParameters) extends SLMemoryModel[TLChannel, UInt] {
  val mem_model = new TLMemoryModel(p)
  var mem_model_state = TLMemoryModel.State.empty()
  // Maps source to remaining burst
  val dataBuffer = new HashMap[Int, Queue[UInt]]()

  // Uses the optimized memory state for TL
  def model(input: Seq[TLChannel]): Seq[Option[SLMemoryState[UInt]]] = {
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

    var result = Seq[Option[SLMemoryState[UInt]]]()
    for (txn <- input) {
      txn match {
        case txnd: TLBundleD =>
          if (txnd.opcode.litValue() == TLOpcodes.AccessAckData) {
            result = result :+ Some(new SLOptTLMemoryState(dataBuffer(txnd.source.litValue().toInt).dequeue()))
          } else {
            result = result :+ None
          }
        case _ => result = result :+ None
      }
    }

    result
  }
}
