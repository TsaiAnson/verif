package verif

import chisel3._
import scala.collection.mutable.HashMap

trait SLMemoryState[M] {
  def get(addr: Int): M
}

// Example of a generic UInt memory state
class SLUIntMemoryState(init: HashMap[Int, UInt] = new HashMap[Int, UInt]()) extends SLMemoryState[UInt] {
  // Non-destructive
  val int_state = init.clone()

  def get(addr: Int): UInt = {
    int_state(addr)
  }
}

trait SLMemoryModel[T,M] {
  def model(input: Seq[T]): Seq[Option[SLMemoryState[M]]]
}

// See TileLink Project for example of TLSLMemoryModel
