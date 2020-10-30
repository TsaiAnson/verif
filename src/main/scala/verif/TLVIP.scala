package verif

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chiseltest._
import firrtl.FirrtlProtos.Firrtl.Statement.Register
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, SimpleDevice}
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.tilelink._
import chisel3.experimental.BundleLiterals._

import scala.collection.mutable
import scala.collection.mutable.Queue

sealed trait TLTransaction
case class Get(addr: UInt) extends TLTransaction {
  override def equals(that: Any): Boolean = {
    that match {
      case that: Get => {
        that.canEqual(this) &&
          this.addr.litValue() == that.addr.litValue()
      }
      case _ => false
    }
  }
}
case class FullPut(addr: UInt, data: UInt) extends TLTransaction {
  override def equals(that: Any): Boolean = {
    that match {
      case that: FullPut => {
        that.canEqual(this) &&
          this.addr.litValue() == that.addr.litValue() &&
          this.data.litValue() == that.data.litValue()
      }
      case _ => false
    }
  }
}
case class AccessAck() extends TLTransaction {
  override def equals(that: Any): Boolean = {
    that match {
      case that: AccessAck => {
        true
      }
      case _ => false
    }
  }
}
case class AccessAckData(data: UInt) extends TLTransaction {
  override def equals(that: Any): Boolean = {
    that match {
      case that: AccessAckData => {
        that.canEqual(this) &&
          this.data.litValue() == that.data.litValue()
      }
      case _ => false
    }
  }
}

trait VerifTLBase {
  def verifTLUBundleParams: TLBundleParameters = TLBundleParameters(addressBits = 64, dataBits = 64, sourceBits = 1,
    sinkBits = 1, sizeBits = 6,
    echoFields = Seq(), requestFields = Seq(), responseFields = Seq(),
    hasBCE = false)
//  def verifTLCBundleParams: TLBundleParameters = TLBundleParameters(addressBits = 64, dataBits = 64, sourceBits = 1,
//    sinkBits = 1, sizeBits = 6,
//    echoFields = Seq(), requestFields = Seq(), responseFields = Seq(),
//    hasBCE = true)

  def TLUBundleAHelper (opcode: UInt = 0.U, param: UInt = 0.U, size: UInt = 2.U, source: UInt = 1.U, address: UInt = 0.U,
                       mask: UInt = 0xff.U, data: UInt = 0.U) : TLBundleA = {
    new TLBundleA(verifTLUBundleParams).Lit(_.opcode -> opcode, _.param -> param, _.size -> size, _.source -> source,
      _.address -> address, _.mask -> mask, _.data -> data)
  }

  def TLUBundleBHelper (opcode: UInt = 0.U, param: UInt = 0.U, size: UInt = 2.U, source: UInt = 1.U, address: UInt = 0.U,
                       mask: UInt = 0xff.U, data: UInt = 0.U) : TLBundleB = {
    new TLBundleB(verifTLUBundleParams).Lit(_.opcode -> opcode, _.param -> param, _.size -> size, _.source -> source,
      _.address -> address, _.mask -> mask, _.data -> data)
  }

  def TLUBundleCHelper (opcode: UInt = 0.U, param: UInt = 0.U, size: UInt = 2.U, source: UInt = 1.U, address: UInt = 0.U,
                        data: UInt = 0.U, corrupt: Bool = false.B) : TLBundleC = {
    new TLBundleC(verifTLUBundleParams).Lit(_.opcode -> opcode, _.param -> param, _.size -> size, _.source -> source,
      _.address -> address, _.data -> data, _.corrupt -> corrupt)
  }

  def TLUBundleDHelper (opcode: UInt = 0.U, param: UInt = 0.U, size: UInt = 2.U, source: UInt = 1.U, sink: UInt = 0.U,
                        data: UInt = 0.U, corrupt: Bool = false.B) : TLBundleD = {
    new TLBundleD(verifTLUBundleParams).Lit(_.opcode -> opcode, _.param -> param, _.size -> size, _.source -> source,
      _.sink -> sink, _.data -> data, _.corrupt -> corrupt)
  }

  def TLUBundleEHelper (sink: UInt = 0.U) : TLBundleE = {
    new TLBundleE(verifTLUBundleParams).Lit(_.sink -> sink)
  }

  def TLBundletoTLTransaction(bnd : TLChannel) : TLTransaction = {
    bnd match {
      case _: TLBundleA =>
        val bndc = bnd.asInstanceOf[TLBundleA]
        if (bndc.opcode.litValue() == 0) {
          FullPut(addr = bndc.address, data = bndc.data)
        } else { // Assuming only two opcodes, 0 and 4
          Get(addr = bndc.address)
        }
      case _: TLBundleD =>
        val bndc = bnd.asInstanceOf[TLBundleD]
        // Need to figure out how to determine AccessAck vs AccessAckData
        AccessAckData(data = bndc.data)
    }
  }

  def TLTransactiontoTLBundle(txn : TLTransaction) : TLChannel = {
    txn match {
      case _: FullPut =>
        val txnc = txn.asInstanceOf[FullPut]
        TLUBundleAHelper(opcode = 0.U, address = txnc.addr, data = txnc.data)
      case _: Get =>
        val txnc = txn.asInstanceOf[Get]
        TLUBundleAHelper(opcode = 4.U, address = txnc.addr)
      case _: AccessAck =>
        TLUBundleDHelper()
      case _: AccessAckData =>
        val txnc = txn.asInstanceOf[AccessAckData]
        TLUBundleDHelper(data = txnc.data)
    }
  }
}

// Used to interface with Slave Nodes
// Does not fully support TL-C yet
trait VerifTLSlaveFunctions extends VerifTLBase {
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

    dC.ready.poke(true.B)

    while(!dC.valid.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)
    dC.ready.poke(false.B)

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

// Used to interface with Master Nodes
trait VerifTLMasterFunctions extends VerifTLBase {
//  // One strategy is to have a register node as slave to drive master
//  // Would need use TLXBar to connect multiple masters
//  def regMan: TLRegisterNode

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

// Basic Driver -- no cycle tracking, only AChannel as Input
class TLSlaveDriverBasic(clock: Clock, interface: TLBundle) extends VerifTLSlaveFunctions {
  val clk = clock
  val TLChannels = interface

  val inputTransactions = Queue[TLTransaction]()

  def push(tx: Seq[TLTransaction]): Unit = {
    for (t <- tx) {
      inputTransactions += t
    }
  }

  fork {
    while (true) {
      if (!inputTransactions.isEmpty) {
        val t = inputTransactions.dequeue()
        writeA(TLTransactiontoTLBundle(t).asInstanceOf[TLBundleA])
        clock.step()
      } else {
        clock.step()
      }
    }
  }
}

// Basic Monitor -- no cycle tracking, only DChannel as Output
class TLSlaveMonitorBasic(clock: Clock, interface: TLBundle) extends VerifTLSlaveFunctions {
  val clk = clock
  val TLChannels = interface

  val txns = Queue[TLTransaction]()

  def getMonitoredTransactions: mutable.MutableList[TLTransaction] = {
    txns
  }

  def clearMonitoredTransactions(): Unit = {
    txns.clear()
  }

  fork {
    // Reads everything
    while (true) {
      txns += TLBundletoTLTransaction(readD())
      clock.step()
    }
  }
}

// Basic Driver -- no cycle tracking, only AChannel as Input
// Interface must be Master
// TODO Allow user to write transactions to fill in "regMap"
// WIP, currently just a hardcoded example
class TLMasterDriverBasic(clock: Clock, interface: TLBundle) extends VerifTLMasterFunctions {
  // Acting like "regmap"
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

  // Process function currently only takes opcode 4 (GET)
  def process(a : TLBundleA) : Unit = {
    txns += TLBundletoTLTransaction(a)

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

    writeD(TLUBundleDHelper(a.opcode, a.param, a.size, a.source, 0.U, result, false.B))
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

// Basic Monitor -- no cycle tracking, currently only records the requests from the master
class TLMasterMonitorBasic(clock: Clock, interface: TLBundle) extends VerifTLMasterFunctions {
  val clk = clock
  val TLChannels = interface

  val txns = Queue[TLTransaction]()

  def process(a: TLBundleA) : Unit = {
    txns += TLBundletoTLTransaction(a)
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
