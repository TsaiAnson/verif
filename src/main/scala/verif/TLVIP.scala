package verif

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chiseltest._
import firrtl.FirrtlProtos.Firrtl.Statement.Register
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, SimpleDevice, TransferSizes}
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.tilelink._
import chisel3.experimental.BundleLiterals._
import chisel3.util.isPow2

import scala.collection.mutable
import scala.collection.mutable.Queue

trait Transaction { this: Bundle =>
  override def equals(that: Any): Boolean = {
    var result = this.getClass() == that.getClass()
    if (result) {
      that.asInstanceOf[Bundle].getElements.zipWithIndex.foreach { t : (Data, Int) =>
        result &= (this.getElements(t._2).litValue() == t._1.litValue())
      }
    }
    result
  }

  override def toString(): String = {
    var result = this.className
    if (this.getElements.size > 0) {
      result += "("
      this.getElements.foreach { t: Data =>
        result += t.litValue().toString() + ", "
      }
      result = result.slice(0, result.length - 2) + ")"
    }
    result
  }
}

// TODO Add source/sink fields when working with buses
sealed trait TLTransaction extends Bundle with Transaction
case class Get(addr: UInt) extends TLTransaction
case class PutFull(addr: UInt, data: UInt) extends TLTransaction
case class PutPartial(addr: UInt, mask: UInt, data: UInt) extends TLTransaction
// TODO: Add denied field
case class AccessAck() extends TLTransaction
case class AccessAckData(data: UInt) extends TLTransaction

trait VerifTLBase {
  // Temporary location for parameters
  def standaloneSlaveParams: TLSlavePortParameters = TLSlavePortParameters.v1(Seq(TLSlaveParameters.v1(address = Seq(AddressSet(0x0, 0xfff)),
    supportsGet = TransferSizes(1, 8), supportsPutFull = TransferSizes(1,8))), beatBytes = 8)
  def standaloneMasterParams: TLMasterPortParameters = TLMasterPortParameters.v1(Seq(TLMasterParameters.v1("bundleBridgeToTL")))
  def verifTLBundleParams: TLBundleParameters = TLBundleParameters(standaloneMasterParams, standaloneSlaveParams)

//  def verifTLUBundleParams: TLBundleParameters = TLBundleParameters(addressBits = 64, dataBits = 64, sourceBits = 1,
//    sinkBits = 1, sizeBits = 6,
//    echoFields = Seq(), requestFields = Seq(), responseFields = Seq(),
//    hasBCE = false)
//  def verifTLCBundleParams: TLBundleParameters = TLBundleParameters(addressBits = 64, dataBits = 64, sourceBits = 1,
//    sinkBits = 1, sizeBits = 6,
//    echoFields = Seq(), requestFields = Seq(), responseFields = Seq(),
//    hasBCE = true)

  def TLUBundleAHelper (opcode: UInt = 0.U, param: UInt = 0.U, size: UInt = 3.U, source: UInt = 1.U, address: UInt = 0.U,
                       mask: UInt = 0xff.U, data: UInt = 0.U, corrupt: Bool = false.B) : TLBundleA = {
    new TLBundleA(verifTLBundleParams).Lit(_.opcode -> opcode, _.param -> param, _.size -> size, _.source -> source,
      _.address -> address, _.mask -> mask, _.data -> data, _.corrupt -> corrupt)
  }

  def TLUBundleBHelper (opcode: UInt = 0.U, param: UInt = 0.U, size: UInt = 2.U, source: UInt = 1.U, address: UInt = 0.U,
                       mask: UInt = 0xff.U, data: UInt = 0.U, corrupt: Bool = false.B) : TLBundleB = {
    new TLBundleB(verifTLBundleParams).Lit(_.opcode -> opcode, _.param -> param, _.size -> size, _.source -> source,
      _.address -> address, _.mask -> mask, _.data -> data, _.corrupt -> corrupt)
  }

  def TLUBundleCHelper (opcode: UInt = 0.U, param: UInt = 0.U, size: UInt = 2.U, source: UInt = 1.U, address: UInt = 0.U,
                        data: UInt = 0.U, corrupt: Bool = false.B) : TLBundleC = {
    new TLBundleC(verifTLBundleParams).Lit(_.opcode -> opcode, _.param -> param, _.size -> size, _.source -> source,
      _.address -> address, _.data -> data, _.corrupt -> corrupt)
  }

  def TLUBundleDHelper (opcode: UInt = 0.U, param: UInt = 0.U, size: UInt = 3.U, source: UInt = 1.U, sink: UInt = 0.U,
                        data: UInt = 0.U, denied: Bool = false.B, corrupt: Bool = false.B) : TLBundleD = {
    new TLBundleD(verifTLBundleParams).Lit(_.opcode -> opcode, _.param -> param, _.size -> size, _.source -> source,
      _.sink -> sink, _.data -> data, _.denied -> denied, _.corrupt -> corrupt)
  }

  def TLUBundleEHelper (sink: UInt = 0.U) : TLBundleE = {
    new TLBundleE(verifTLBundleParams).Lit(_.sink -> sink)
  }

  // Helper functions for message checking
  def aligned(data : UInt, base : UInt) : Boolean = {
    val dataI = data.litValue()
    val baseI = base.litValue() - 1
    ((dataI & baseI) == 0) && contiguous(baseI.U)
  }

  def alignedLg(data : UInt, base : UInt) : Boolean = {
    aligned(data, (1 << base.litValue().toInt).U)
  }

  def contiguous(data : UInt) : Boolean = {
    val dataI = data.litValue()
    ((dataI + 1) & ~dataI) == (dataI + 1)
  }

  def contains(sizes: TransferSizes, x: UInt) : Boolean = {
    (x.litValue() >= sizes.min && x.litValue() <= sizes.max && isPow2(x.litValue()))
  }

  def containsLg(sizes: TransferSizes, lg: UInt) : Boolean = {
    contains(sizes, (1 << lg.litValue().toInt).U)
  }

  // Throws assertions if DUT responds with incorrect fields
  // NOTE: Currently only supports TL-UL
  def TLBundletoTLTransaction(bnd : TLChannel, TLSParam : TLSlaveParameters = standaloneSlaveParams.managers(0) ) : TLTransaction = {
    bnd match {
      case _: TLBundleA =>
        val bndc = bnd.asInstanceOf[TLBundleA]
        if (bndc.opcode.litValue() == 0) {

          assert(TLSParam.supportsPutFull != TransferSizes.none, "Channel does not support PUTFULL requests.")
          assert(bndc.param.litValue() == 0, "Non-zero param field for PUTFULL TLBundle")
          assert(containsLg(TLSParam.supportsPutFull, bndc.size), "Size is outside of valid transfer sizes")
          assert(alignedLg(bndc.address, bndc.size), s"PUTFULL Address (${bndc.address}) is NOT aligned with size (${bndc.size})")
          assert(alignedLg(bndc.mask, bndc.size), s"PUTFULL MASK (${bndc.mask}) is not aligned with size (${bndc.size})")
          assert(contiguous(bndc.mask), "PUTFULL MASK is not contiguous")
          PutFull(addr = bndc.address, data = bndc.data)

        } else if (bndc.opcode.litValue() == 1) {

          assert(TLSParam.supportsPutPartial != TransferSizes.none, "Channel does not support PUTPARTIAL requests.")
          assert(bndc.param.litValue() == 0, "Non-zero param field for PUTPARTIAL TLBundle")
          assert(containsLg(TLSParam.supportsPutPartial, bndc.size), "Size is outside of valid transfer sizes")
          assert(alignedLg(bndc.address, bndc.size), s"PUTPARTIAL Address (${bndc.address}) is NOT aligned with size (${bndc.size})")
          // TODO Check that high bits are aligned
          PutPartial(addr = bndc.address, mask = bndc.mask, data = bndc.data)

        } else if (bndc.opcode.litValue() == 4) {

          assert(TLSParam.supportsGet != TransferSizes.none, "Channel does not support GET requests.")
          assert(bndc.param.litValue() == 0, "Non-zero param field for GET TLBundle")
          assert(containsLg(TLSParam.supportsGet, bndc.size), "Size is outside of valid transfer sizes")
          // Need to check
//          assert(alignedLg(bndc.mask, bndc.size), "GET MASK is not aligned")
          assert(contiguous(bndc.mask), "GET MASK is not contiguous")
          assert(!bndc.corrupt.litToBoolean, "Corrupt GET TLBundle")
          Get(addr = bndc.address)

        } else {

          assert(false, "Invalid OPCODE on A Channel")
          Get(addr = 0.U)

        }
      case _: TLBundleD =>
        val bndc = bnd.asInstanceOf[TLBundleD]
        if (bndc.opcode.litValue() == 0) {

          assert(bndc.param.litValue() == 0, "Non-zero param field for ACCESSACK TLBundle")
          assert(!bndc.corrupt.litToBoolean, "Corrupt ACCESSACK TLBundle")
          AccessAck()

        } else if (bndc.opcode.litValue() == 1) {

          assert(bndc.param.litValue() == 0, "Non-zero param field for ACCESSACKDATA TLBundle")
          if (bndc.denied.litToBoolean) {
            assert(bndc.corrupt.litToBoolean, "ACCESSACKDATA denied but not corrupt")
          }
          AccessAckData(data = bndc.data)

        } else {

          assert(false, "Invalid OPCODE on D Channel")
          AccessAck()

        }
    }
  }

  def TLTransactiontoTLBundle(txn : TLTransaction) : TLChannel = {
    txn match {
      case _: PutFull =>
        val txnc = txn.asInstanceOf[PutFull]
        TLUBundleAHelper(opcode = 0.U, address = txnc.addr, data = txnc.data)
      case _: PutPartial =>
        val txnc = txn.asInstanceOf[PutPartial]
        TLUBundleAHelper(opcode = 0.U, address = txnc.addr, mask = txnc.mask, data = txnc.data)
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

// Functions for TL Master VIP
// Currently supports TL-UL, (TL-UH)
trait VerifTLMasterFunctions extends VerifTLBase {
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

    // Quick fix:
    // Removed for monitor (should not poke interface)
//    dC.ready.poke(true.B)

    while(!dC.valid.peek().litToBoolean) {
      clk.step(1)
    }
    clk.step(1)
//    dC.ready.poke(false.B)

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
trait VerifTLSlaveFunctions extends VerifTLBase {
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

  val inputTransactions = Queue[TLTransaction]()

  def push(tx: Seq[TLTransaction]): Unit = {
    for (t <- tx) {
      inputTransactions += t
    }
  }

  fork {
    // Ready always high (TL monitor always receiving in transactions)
    interface.d.ready.poke(true.B)
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

// TLMonitor acting as a Master node
class TLMonitorMaster(clock: Clock, interface: TLBundle) extends VerifTLMasterFunctions {
  val clk = clock
  val TLChannels = interface

  val txns = Queue[TLTransaction]()

  def getMonitoredTransactions: mutable.MutableList[TLTransaction] = {
    txns
  }

  def clearMonitoredTransactions(): Unit = {
    txns.clear()
  }

  fork.withRegion(Monitor) {
    // Reads everything
    while (true) {
      txns += TLBundletoTLTransaction(readD())
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
    txns += TLBundletoTLTransaction(a)

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
