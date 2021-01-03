package verif

import chisel3._

object TestBundles {
  case class A(w: Int) extends Bundle with Transaction {
    val x = UInt(w.W)
    val y = UInt(w.W)
  }
  case class K() extends Bundle with Transaction {
    val x = UInt(8.W)
    val y = UInt(8.W)
    val b = UInt(1.W)
  }
}
