package verif

import chisel3._
import chiseltest._
import freechips.rocketchip.tilelink._
import scala.collection.mutable
import scala.collection.mutable.{Queue,ListBuffer}
import VerifTLUtils._

// Functions for TL Master VIP
// Currently supports TL-UL, (TL-UH)
trait VerifTLMasterFunctions {
  def clk: Clock
  def TLChannels: TLBundle


  def pokeA(a: TLBundleA): Unit = {
    val aC = TLChannels.a
    aC.bits.opcode.poke(a.opcode)
    aC.bits.param.poke(a.param)
    aC.bits.size.poke(a.size)
    aC.bits.source.poke(a.source)
    aC.bits.address.poke(a.address)
    aC.bits.mask.poke(a.mask)
    aC.bits.data.poke(a.data)
  }

  def peekB(): TLBundleB = {
    val bC = TLChannels.b
    val opcode = bC.bits.opcode.peek()
    val param = bC.bits.param.peek()
    val size = bC.bits.size.peek()
    val source = bC.bits.source.peek()
    val address = bC.bits.address.peek()
    val mask = bC.bits.mask.peek()
    val data = bC.bits.data.peek()

    TLUBundleBHelper(opcode, param, size, source, address, mask, data)
  }

  def pokeC(c: TLBundleC): Unit = {
    val cC = TLChannels.c
    cC.bits.opcode.poke(c.opcode)
    cC.bits.param.poke(c.param)
    cC.bits.size.poke(c.size)
    cC.bits.source.poke(c.source)
    cC.bits.address.poke(c.address)
    cC.bits.data.poke(c.data)
    cC.bits.corrupt.poke(c.corrupt)
  }

  def peekD(): TLBundleD = {
    val dC = TLChannels.d
    val opcode = dC.bits.opcode.peek()
    val param = dC.bits.param.peek()
    val size = dC.bits.size.peek()
    val source = dC.bits.source.peek()
    val sink = dC.bits.sink.peek()
    val data = dC.bits.data.peek()
    val corrupt = dC.bits.corrupt.peek()

    TLUBundleDHelper(opcode, param, size, source, sink, data, corrupt)
  }

  def pokeE(e: TLBundleE): Unit = {
    val eC = TLChannels.e
    eC.bits.sink.poke(e.sink)
  }

  def writeA(a: TLBundleA): Unit = {
    val aC = TLChannels.a

    aC.valid.poke(true.B)
    pokeA(a)

    while(!aC.ready.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)

    aC.valid.poke(false.B)
  }

  def readB(): TLBundleB = {
    val bC = TLChannels.b

    bC.ready.poke(true.B)

    while(!bC.valid.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)
    bC.ready.poke(false.B)

    peekB()
  }

  def writeC(c: TLBundleC): Unit = {
    val cC = TLChannels.c

    cC.valid.poke(true.B)
    pokeC(c)

    while(!cC.ready.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)

    cC.valid.poke(false.B)
  }

  def readD(): TLBundleD = {
    val dC = TLChannels.d

    while(!dC.valid.peek().litToBoolean) {
      clk.step(1)
    }

    peekD()
  }

  def writeE(e: TLBundleE): Unit = {
    val eC = TLChannels.e

    eC.valid.poke(true.B)
    pokeE(e)

    while(!eC.ready.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)

    eC.valid.poke(false.B)
  }

  // TODO Figure out why poking C and E does not work
  def reset(): Unit = {
    pokeA(TLUBundleAHelper())
//    pokeC(TLUBundleCHelper())
//    pokeE(TLUBundleEHelper())
    TLChannels.a.valid.poke(false.B)
    TLChannels.b.ready.poke(false.B)
    TLChannels.c.valid.poke(false.B)
    TLChannels.d.ready.poke(false.B)
    TLChannels.e.valid.poke(false.B)
  }
}

// Functions for TL Slave VIP
// Currently supports TL-UL, (TL-UH)
trait VerifTLSlaveFunctions {
  def clk: Clock
  def TLChannels: TLBundle

  def peekA(): TLBundleA = {
    val aC = TLChannels.a
    val opcode = aC.bits.opcode.peek()
    val param = aC.bits.param.peek()
    val size = aC.bits.size.peek()
    val source = aC.bits.source.peek()
    val address = aC.bits.address.peek()
    val mask = aC.bits.mask.peek()
    val data = aC.bits.data.peek()

    TLUBundleAHelper(opcode, param, size, source, address, mask, data)
  }

  def pokeB(b: TLBundleB): Unit = {
    val bC = TLChannels.b
    bC.bits.opcode.poke(b.opcode)
    bC.bits.param.poke(b.param)
    bC.bits.size.poke(b.size)
    bC.bits.source.poke(b.source)
    bC.bits.address.poke(b.address)
    bC.bits.mask.poke(b.mask)
    bC.bits.data.poke(b.data)
  }

  def peekC(): TLBundleC = {
    val cC = TLChannels.c
    val opcode = cC.bits.opcode.peek()
    val param = cC.bits.param.peek()
    val size = cC.bits.size.peek()
    val source = cC.bits.source.peek()
    val address = cC.bits.address.peek()
    val data = cC.bits.data.peek()
    val corrupt = cC.bits.corrupt.peek()

    TLUBundleCHelper(opcode, param, size, source, address, data, corrupt)
  }

  def pokeD(d: TLBundleD): Unit = {
    val dC = TLChannels.d
    dC.bits.opcode.poke(d.opcode)
    dC.bits.param.poke(d.param)
    dC.bits.size.poke(d.size)
    dC.bits.source.poke(d.source)
    dC.bits.sink.poke(d.sink)
    dC.bits.data.poke(d.data)
    dC.bits.corrupt.poke(d.corrupt)
  }

  def peekE(): TLBundleE = {
    val eC = TLChannels.e
    val sink = eC.bits.sink.peek()

    TLUBundleEHelper(sink)
  }

  def readA(): TLBundleA = {
    val aC = TLChannels.a

    aC.ready.poke(true.B)

    while(!aC.valid.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)
    aC.ready.poke(false.B)

    peekA()
  }

  def writeB(b: TLBundleB): Unit = {
    val bC = TLChannels.b

    bC.valid.poke(true.B)
    pokeB(b)

    while(!bC.ready.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)

    bC.valid.poke(false.B)
  }

  def readC(): TLBundleC = {
    val cC = TLChannels.c

    cC.ready.poke(true.B)

    while(!cC.valid.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)
    cC.ready.poke(false.B)

    peekC()
  }

  def writeD(d: TLBundleD): Unit = {
    val dC = TLChannels.d

    dC.valid.poke(true.B)
    pokeD(d)

    while(!dC.ready.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)

    dC.valid.poke(false.B)
  }

  def readE(): TLBundleE = {
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
//    pokeB(TLUBundleBHelper())
    pokeD(TLUBundleDHelper())
    TLChannels.a.ready.poke(false.B)
//    TLChannels.b.valid.poke(false.B)
//    TLChannels.c.ready.poke(false.B)
    TLChannels.d.valid.poke(false.B)
//    TLChannels.e.ready.poke(false.B)
  }

  def process(req: TLBundleA): Unit
}

// TLDriver acting as a Master node
class TLDriverMaster(clock: Clock, interface: TLBundle) extends VerifTLMasterFunctions {
  val clk = clock
  val TLChannels = interface

  val inputTransactions = Queue[TLChannel]()

  def push(tx: Seq[TLTransaction]): Unit = {
    for (t <- tx) {
      inputTransactions ++= TLTransactiontoTLBundles(t)
    }
  }

  fork {
    // Ready always high (TL monitor always receiving in transactions)
    interface.d.ready.poke(true.B)
    while (true) {
      if (!inputTransactions.isEmpty) {
        val t = inputTransactions.dequeue()
        writeA(t.asInstanceOf[TLBundleA])
        clock.step()
      } else {
        clock.step()
      }
    }
  }
}

// TLMonitor acting as a Master node
class TLMonitorMaster(clock: Clock, interface: TLBundle) extends VerifTLMasterFunctions {
  val clk = clock
  val TLChannels = interface

  val txns = Queue[TLChannel]()

  def getMonitoredTransactions: List[TLTransaction] = {
    val result = new ListBuffer[TLTransaction]
    for (g <- groupTLBundles(txns.toList)) {
      result += TLBundlestoTLTransaction(g)
    }
    result.toList
  }

  def clearMonitoredTransactions(): Unit = {
    txns.clear()
  }

  fork.withRegion(Monitor) {
    // Reads everything
    while (true) {
      txns += readD()
      clock.step()
    }
  }
}

// TLDriver acting as a Slave node
// TODO Currently drives DChannel (results)
class TLDriverSlave(clock: Clock, interface: TLBundle) extends VerifTLSlaveFunctions {
  // Acting like "regmap"
  // TODO: Add byte-level addressing
  var hash = mutable.HashMap(0 -> 10, 0x08 -> 11, 0x10 -> 12, 0x18 -> 13)

  val clk = clock

  val TLChannels = interface

  val txns = Queue[TLTransaction]()

  def getMonitoredTransactions: mutable.MutableList[TLTransaction] = {
//    for (x <- hash.keys) {
//      print(s"(${x}, ${hash(x)}), ")
//    }
//    println("")
    txns
  }

  // Process function currently only takes opcode 0 (FullPut) and 4 (Get)
  // TODO: Implement PartialPut requests
  def process(a : TLBundleA) : Unit = {
    txns += TLBundletoTLTransactionOLD(a)

    if (!(a.opcode.litValue() == 4 || a.opcode.litValue() == 0)) {
      println(s"ONLY FULL-PUT (0) AND GET (4) OPCODE IS PERMITTED. GIVEN OP: ${a.opcode} EXAMPLE TEST.")
    }

    var result = 0.U
    var opcode = 0.U
    var corrupt = false.B
    if (a.opcode.litValue() == 0) {
      hash(a.address.litValue().toInt) = a.data.litValue().toInt
      result = a.data
    } else if (a.opcode.litValue() == 4) {
      if (hash.contains(a.address.litValue().toInt)) {
        result = hash(a.address.litValue().toInt).U
      } else {
        result = 0.U
      }
      opcode = 1.U
    } else {
      assert(false, s"Unknown opcode: ${a.opcode.litValue()}")
      // Marking corrupt as indicator
      corrupt = true.B
    }

    writeD(TLUBundleDHelper(opcode, a.param, a.size, a.source, 0.U, result, corrupt))
  }

  // Currently just processes the requests from master
  fork {
    reset()
    while (true) {
      process(readA())
      clk.step()
    }
  }
}

// TLMonitor acting as a Slave node
class TLMonitorSlave(clock: Clock, interface: TLBundle) extends VerifTLSlaveFunctions {
  val clk = clock
  val TLChannels = interface

  val txns = Queue[TLTransaction]()

  def process(a: TLBundleA) : Unit = {
    txns += TLBundletoTLTransactionOLD(a)
  }

  def getMonitoredTransactions: mutable.MutableList[TLTransaction] = {
    txns
  }

  fork {
    // Reads all requests
    while (true) {
      process(readA())
      clock.step()
    }
  }
}
