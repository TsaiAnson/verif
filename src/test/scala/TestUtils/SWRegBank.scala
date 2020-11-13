package TestUtils

import chisel3._
import verif._

import scala.collection.mutable.{HashMap, ListBuffer}

class SWRegBank(regCount : Int = 8, regSizeBytes : Int = 8) {
  // Quick HashMap hack that only works for aligned memory
  // TODO: Implement byte-level memory
  val internalBank = HashMap[Int, Int]()

  def process (txns : Seq[TLTransaction]) : Seq[TLTransaction] = {
    val results = ListBuffer[TLTransaction]()

    for (txn <- txns) {
      txn match {
        case _: Get =>
          val txni = txn.asInstanceOf[Get]
          val addr = txni.addr.litValue().toInt

          // Legal Address checking (only for aligned addresses)
          if (addr > regCount * regSizeBytes || addr % regSizeBytes != 0) {
            throw new RuntimeException(s"ILLEGAL GET ADDRESS: ${addr}")
          }

          if (internalBank.contains(addr)) {
            results += AccessAckData(data = internalBank(addr).U(64.W))
          } else {
            results += AccessAckData(data = 0.U(64.W))
          }
        case _: FullPut =>
          val txni = txn.asInstanceOf[FullPut]
          val addr = txni.addr.litValue().toInt

          // Legal Address checking (only for aligned addresses)
          if (addr > regCount * regSizeBytes || addr % regSizeBytes != 0) {
            throw new RuntimeException(s"ILLEGAL FULLPUT ADDRESS: ${addr}")
          }

          internalBank(addr) = txni.data.litValue().toInt
          results += AccessAckData(data = txni.data)
      }
    }

    results
  }
}
