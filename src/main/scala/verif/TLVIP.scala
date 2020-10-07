package verif

import org.scalatest._
import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, VerilatorBackendAnnotation, WriteVcdAnnotation}
import freechips.rocketchip.tilelink._

import scala.collection.mutable
import scala.collection.mutable.Queue

case class VerifTLAChannel(opcode:  UInt = 0.U,
                           param:   UInt = 0.U,
                           size:    UInt = 2.U,
                           source:  UInt = 1.U,
                           address: UInt = 0.U,
                           mask:    UInt = 0xff.U,
                           data:    UInt = 0.U) extends Bundle

case class VerifTLBChannel(opcode:  UInt = 0.U,
                           param:   UInt = 0.U,
                           size:    UInt = 0.U,
                           source:  UInt = 0.U,
                           address: UInt = 0.U,
                           mask:    UInt = 0.U,
                           data:    UInt = 0.U) extends Bundle

case class VerifTLCChannel(opcode:  UInt = 0.U,
                           param:   UInt = 0.U,
                           size:    UInt = 2.U,
                           source:  UInt = 1.U,
                           address: UInt = 0.U,
                           data:    UInt = 0.U,
                           corrupt: Bool = false.B) extends Bundle

case class VerifTLDChannel(opcode:  UInt = 0.U,
                           param:   UInt = 0.U,
                           size:    UInt = 2.U,
                           source:  UInt = 1.U,
                           sink:    UInt = 0.U,
                           data:    UInt = 0.U,
                           corrupt: Bool = false.B) extends Bundle

case class VerifTLEChannel(sink:  UInt = 0.U) extends Bundle

// Used to interface with Master Nodes
// Does not fully support TL-C yet
trait VerifTLMasterModel {
  def clk: Clock
  def TLChannels: TLBundle


  def pokeA(a: VerifTLAChannel): Unit = {
    val aC = TLChannels.a
    aC.bits.opcode.poke(a.opcode)
    aC.bits.param.poke(a.param)
    aC.bits.size.poke(a.size)
    aC.bits.source.poke(a.source)
    aC.bits.address.poke(a.address)
    aC.bits.mask.poke(a.mask)
    aC.bits.data.poke(a.data)
  }

  def peekB(): VerifTLBChannel = {
    val bC = TLChannels.b
    val opcode = bC.bits.opcode.peek()
    val param = bC.bits.param.peek()
    val size = bC.bits.size.peek()
    val source = bC.bits.source.peek()
    val address = bC.bits.address.peek()
    val mask = bC.bits.mask.peek()
    val data = bC.bits.data.peek()

    VerifTLBChannel(opcode, param, size, source, address, mask, data)
  }

  def pokeC(c: VerifTLCChannel): Unit = {
    val cC = TLChannels.c
    cC.bits.opcode.poke(c.opcode)
    cC.bits.param.poke(c.param)
    cC.bits.size.poke(c.size)
    cC.bits.source.poke(c.source)
    cC.bits.address.poke(c.address)
    cC.bits.data.poke(c.data)
    cC.bits.corrupt.poke(c.corrupt)
  }

  def peekD(): VerifTLDChannel = {
    val dC = TLChannels.d
    val opcode = dC.bits.opcode.peek()
    val param = dC.bits.param.peek()
    val size = dC.bits.size.peek()
    val source = dC.bits.source.peek()
    val sink = dC.bits.sink.peek()
    val data = dC.bits.data.peek()
    val corrupt = dC.bits.corrupt.peek()

    VerifTLDChannel(opcode, param, size, source, sink, data, corrupt)
  }

  def pokeE(e: VerifTLEChannel): Unit = {
    val eC = TLChannels.e
    eC.bits.sink.poke(e.sink)
  }

  def writeA(a: VerifTLAChannel): Unit = {
    val aC = TLChannels.a

    aC.valid.poke(true.B)
    pokeA(a)

    while(aC.ready.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)

    aC.valid.poke(false.B)
  }

  def readB(): VerifTLBChannel = {
    val bC = TLChannels.b

    bC.ready.poke(true.B)

    while(!bC.valid.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)
    bC.ready.poke(false.B)

    peekB()
  }

  def writeC(c: VerifTLCChannel): Unit = {
    val cC = TLChannels.c

    cC.valid.poke(true.B)
    pokeC(c)

    while(cC.ready.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)

    cC.valid.poke(false.B)
  }

  def readD(): VerifTLDChannel = {
    val dC = TLChannels.d

    dC.ready.poke(true.B)

    while(!dC.valid.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)
    dC.ready.poke(false.B)

    peekD()
  }

  def writeE(e: VerifTLEChannel): Unit = {
    val eC = TLChannels.e

    eC.valid.poke(true.B)
    pokeE(e)

    while(eC.ready.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)

    eC.valid.poke(false.B)
  }

  def reset(): Unit = {
    pokeA(VerifTLAChannel())
    pokeC(VerifTLCChannel())
    pokeE(VerifTLEChannel())
    TLChannels.a.valid.poke(false.B)
    TLChannels.b.ready.poke(false.B)
    TLChannels.c.valid.poke(false.B)
    TLChannels.d.ready.poke(false.B)
    TLChannels.e.valid.poke(false.B)
  }
}

// Used to interface with Client Nodes
trait VerifTLClientModel {
  // To be implemented
}

// Basic Driver -- no cycle tracking
class TLManagerDriverBasic(clock: Clock, interface: TLBundle) extends VerifTLMasterModel {
  val clk = clock
  val TLChannels = interface

  val inputTransactions = Queue[VerifTLAChannel]()

  def push(tx: Seq[VerifTLAChannel]): Unit = {
    for (t <- tx) {
      inputTransactions += t
    }
  }

  fork {
    while (true) {
      if (!inputTransactions.isEmpty) {
        val t = inputTransactions.dequeue()
        writeA(t)
        clock.step()
      } else {
        clock.step()
      }
    }
  }
}

class TLManagerMonitorBasic(clock: Clock, interface: TLBundle) extends VerifTLMasterModel {
  val clk = clock
  val TLChannels = interface

  val txns = Queue[VerifTLDChannel]()

  def getMonitoredTransactions: mutable.MutableList[VerifTLDChannel] = {
    txns
  }

  def clearMonitoredTransactions(): Unit = {
    txns.clear()
  }

  fork {
    // Reads everything
    while (true) {
      txns += readD()
      clock.step(1)
    }
  }
}
