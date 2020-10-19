package verif

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chiseltest._
import firrtl.FirrtlProtos.Firrtl.Statement.Register
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, SimpleDevice}
import freechips.rocketchip.regmapper.RegField
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

// Used to interface with Manager Nodes
// Does not fully support TL-C yet
trait VerifTLManagerFunctions {
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

  // TODO Figure out why poking C and E does not work
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
trait VerifTLClientFunctions {
//  // One strategy is to have a register node as Manager to drive client
//  // Would need use TLXBar to connect multiple clients
//  def regMan: TLRegisterNode

  def clk: Clock
  def TLChannels: TLBundle

  def peekA(): VerifTLAChannel = {
    val aC = TLChannels.a
    val opcode = aC.bits.opcode.peek()
    val param = aC.bits.param.peek()
    val size = aC.bits.size.peek()
    val source = aC.bits.source.peek()
    val address = aC.bits.address.peek()
    val mask = aC.bits.mask.peek()
    val data = aC.bits.data.peek()

    VerifTLAChannel(opcode, param, size, source, address, mask, data)
  }

  def pokeB(b: VerifTLBChannel): Unit = {
    val bC = TLChannels.b
    bC.bits.opcode.poke(b.opcode)
    bC.bits.param.poke(b.param)
    bC.bits.size.poke(b.size)
    bC.bits.source.poke(b.source)
    bC.bits.address.poke(b.address)
    bC.bits.mask.poke(b.mask)
    bC.bits.data.poke(b.data)
  }

  def peekC(): VerifTLCChannel = {
    val cC = TLChannels.c
    val opcode = cC.bits.opcode.peek()
    val param = cC.bits.param.peek()
    val size = cC.bits.size.peek()
    val source = cC.bits.source.peek()
    val address = cC.bits.address.peek()
    val data = cC.bits.data.peek()
    val corrupt = cC.bits.corrupt.peek()

    VerifTLCChannel(opcode, param, size, source, address, data, corrupt)
  }

  def pokeD(d: VerifTLDChannel): Unit = {
    val dC = TLChannels.d
    dC.bits.opcode.poke(d.opcode)
    dC.bits.param.poke(d.param)
    dC.bits.size.poke(d.size)
    dC.bits.source.poke(d.source)
    dC.bits.sink.poke(d.sink)
    dC.bits.data.poke(d.data)
    dC.bits.corrupt.poke(d.corrupt)
  }

  def peekE(): VerifTLEChannel = {
    val eC = TLChannels.e
    val sink = eC.bits.sink.peek()

    VerifTLEChannel(sink)
  }

  def readA(): VerifTLAChannel = {
    val aC = TLChannels.a

    aC.ready.poke(true.B)

    while(!aC.valid.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)
    aC.ready.poke(false.B)

    peekA()
  }

  def writeB(b: VerifTLBChannel): Unit = {
    val bC = TLChannels.b

    bC.valid.poke(true.B)
    pokeB(b)

    while(bC.ready.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)

    bC.valid.poke(false.B)
  }

  def readC(): VerifTLCChannel = {
    val cC = TLChannels.c

    cC.ready.poke(true.B)

    while(!cC.valid.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)
    cC.ready.poke(false.B)

    peekC()
  }

  def writeD(d: VerifTLDChannel): Unit = {
    val dC = TLChannels.d

    dC.valid.poke(true.B)
    pokeD(d)

    while(dC.ready.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)

    dC.valid.poke(false.B)
  }

  def readE(): VerifTLEChannel = {
    val eC = TLChannels.e

    eC.ready.poke(true.B)

    while(!eC.valid.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)
    eC.ready.poke(false.B)

    peekE()
  }

  // TODO Figure out why pokingB doesn't work
  def reset(): Unit = {
//    pokeB(VerifTLBChannel())
    pokeD(VerifTLDChannel())
    TLChannels.a.ready.poke(false.B)
//    TLChannels.b.valid.poke(false.B)
//    TLChannels.c.ready.poke(false.B)
    TLChannels.d.valid.poke(false.B)
//    TLChannels.e.ready.poke(false.B)
  }

  def process(req: VerifTLAChannel): Unit
}

// Basic Driver -- no cycle tracking, only AChannel as Input
class TLManagerDriverBasic(clock: Clock, interface: TLBundle) extends VerifTLManagerFunctions {
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

// Basic Monitor -- no cycle tracking, only DChannel as Output
class TLManagerMonitorBasic(clock: Clock, interface: TLBundle) extends VerifTLManagerFunctions {
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

// Basic Driver -- no cycle tracking, only AChannel as Input
// Interface must be Client
// TODO Allow user to write transactions to fill in "regMap"
// WIP, currently just a hardcoded example
class TLClientDriverBasic(clock: Clock, interface: TLBundle) extends VerifTLClientFunctions {
  // Acting like "regmap"
  var hash = mutable.HashMap(0 -> 10, 0x08 -> 11, 0x10 -> 12, 0x18 -> 13)

  val clk = clock

  val TLChannels = interface

  val txns = Queue[VerifTLAChannel]()

  def getMonitoredTransactions: mutable.MutableList[VerifTLAChannel] = {
    for (x <- hash.keys) {
      print(s"(${x}, ${hash(x)}), ")
    }
    println("")
    txns
  }

  // Process function currently only takes opcode 4 (GET)
  def process(a : VerifTLAChannel) : Unit = {
    txns += a

    if (!(a.opcode.litValue() == 4 || a.opcode.litValue() == 0)) {
      println(s"ONLY FULL-PUT (0) AND GET (4) OPCODE IS PERMITTED. GIVEN OP: ${a.opcode} EXAMPLE TEST.")
    }

    var result = 0.U;
    if (a.opcode.litValue() == 0) {
      hash(a.address.litValue().toInt) = a.data.litValue().toInt
      result = a.data
    } else {
      if (hash.contains(a.address.litValue().toInt)) {
        result = hash(a.address.litValue().toInt).U
      }
    }

    writeD(VerifTLDChannel(a.opcode, a.param, a.size, a.source, 0.U, result, true.B))
  }

  // Currently just processes the requests from Client
  fork {
    reset()
    while (true) {
      process(readA())
      clk.step(1)
    }
  }
}

// Basic Monitor -- no cycle tracking, currently only records the requests from the client
class TLClientMonitorBasic(clock: Clock, interface: TLBundle) extends VerifTLClientFunctions {
  val clk = clock
  val TLChannels = interface

  val txns = Queue[VerifTLAChannel]()

  def process(a: VerifTLAChannel) : Unit = {
    txns += a
  }

  def getMonitoredTransactions: mutable.MutableList[VerifTLAChannel] = {
    txns
  }

  fork {
    // Reads all requests
    while (true) {
      process(readA())
      clock.step(1)
    }
  }
}
