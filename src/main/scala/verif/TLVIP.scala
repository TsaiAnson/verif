package verif

import org.scalatest._
import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, VerilatorBackendAnnotation, WriteVcdAnnotation}
import chisel3.experimental.BundleLiterals._
import freechips.rocketchip.tilelink._

import scala.collection.mutable
import scala.collection.mutable.Queue

case class TLTX[T <: Data](data: T, waitCycles: UInt = 0.U, postSendCycles: UInt = 0.U, cycleStamp: Int = 0) extends Bundle {
  override def cloneType = TLTX(data, waitCycles, postSendCycles).asInstanceOf[this.type]
}

case class TLTXBasic(address: UInt = 0.U, data: UInt = 0.U) extends Bundle {
  override def cloneType = TLTXBasic(address, data).asInstanceOf[this.type]
}



// Only supports basic TL formats (Non TL-C, BCE False)
// !!!Currently just a skeleton, as I'm still trying to figure out how to write to A Channel
class TLDriverBasic(clock: Clock, interface: Seq[TLBundle]) {
  // Just working with basic right now
  val inputTransactions = Queue[TLTXBasic]()

  def push(tx: Seq[TLTXBasic]): Unit = {
    for (t <- tx) {
      inputTransactions += t
    }
  }

  fork {
    val AChannel = interface.head.a
    while (true) {
      if (!inputTransactions.isEmpty) {
        val t = inputTransactions.dequeue()
        AChannel.bits.address.poke(t.address)
        AChannel.bits.data.poke(t.data)
        AChannel.valid.poke(1.B)
        if (AChannel.ready.peek().litToBoolean) {
          clock.step()
          AChannel.valid.poke(0.B)
        } else {
          clock.step()
        }
      } else {
        clock.step()
      }
    }
  }
}

// Only supports basic TL formats (Non TL-C, BCE False)
// !!!Currently just a skeleton, as I'm still trying to figure out how to read from D Channel
class TLMonitorBasic(clock: Clock, interface: Seq[TLBundle]) {
  val txns = Queue[TLTXBasic]()

  def getMonitoredTransactions: mutable.MutableList[TLTXBasic] = {
    txns
  }

  def clearMonitoredTransactions(): Unit = {
    txns.clear()
  }

  fork {
    val DChannel = interface.head.d
    while (true) {
      DChannel.ready.poke(1.B)
      if (DChannel.valid.peek().litToBoolean) {
        val t = TLTXBasic(data = DChannel.bits.data.peek())
        txns += t
      }
      clock.step()
    }
  }
}
