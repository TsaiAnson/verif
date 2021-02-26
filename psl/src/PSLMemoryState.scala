package verif

import chisel3._
import scala.collection.mutable.HashMap

trait PSLMemoryState[T] {
  def get(addr: Int): T
}

// Example of a generic UInt memory state
class PSLUIntMemoryState(init: HashMap[Int, UInt] = new HashMap[Int, UInt]()) extends PSLMemoryState[UInt] {
  // Non-destructive
  val int_state = init.clone()

  def get(addr: Int): UInt = {
    int_state(addr)
  }
}

// Custom optimized TL memory state (only one address-data pair per transaction)
class PSLOptTLMemoryState(init: UInt = 0.U) extends PSLMemoryState[UInt] {
  var int_state = init

  // Addr unused
  def get(addr: Int): UInt = {
    int_state
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case t: PSLOptTLMemoryState => this.int_state.litValue() == t.int_state.litValue()
      case _ => false
    }
  }
  override def toString: String = s"${this.getClass.getTypeName}: $int_state"
}
