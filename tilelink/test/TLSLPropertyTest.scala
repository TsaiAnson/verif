package verif

import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import TLTransaction._
import SL._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.subsystem.WithoutTLMonitors
import freechips.rocketchip.tilelink.{TLBundleA, TLBundleD, TLBundleParameters, TLChannel}
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.HashMap

class TLSLPropertyTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = new WithoutTLMonitors
  implicit val params: TLBundleParameters = TLBundleParameters(DefaultTLParams.master(), DefaultTLParams.slave)

  it should "sanity test AtmProp and Seq and same cycle checking" in {
    val getAP = qAP({(t: TLBundleA, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) => t.opcode.litValue() == TLOpcodes.Get}, "If is Get request")
    val paramZero = qAP({(t: TLBundleA, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) => t.param.litValue() == 0}, "Parameter must be zero")
    val getParamProp = qProp[TLBundleA, Int, UInt]( getAP + Implies + ###(0) + paramZero)

    // Good Get Transactions
    val inputTransactions = Seq(
      Get(0x0),
      Get(0x08),
      Put(0x0, 0),
      Put(0x08, 1),
      Get(0x0),
      Get(0x08)
    )
    assert(getParamProp.check(inputTransactions))
    getParamProp.getCoverage()
    getParamProp.clearCoverage()

    // One Faulty Get Transaction
    val badGet = new TLBundleA(params).Lit(_.opcode -> TLOpcodes.Get.U, _.param -> 1.U, _.size -> 3.U,
      _.source -> 0.U, _.address -> 0x8.U, _.mask -> 0xff.U, _.corrupt -> 0.B, _.data -> 0.U)
    val inputTransactionsBad = Seq(
      Get(0x0),
      badGet,
      Get(0x10),
      Get(0x18)
    )
    println("Below should fail:")
    assert(!getParamProp.check(inputTransactionsBad))
    println("Done.")
    getParamProp.getCoverage()
  }

  it should "sanity test beat checking in bursts" in {
    // Currently hardcoded for different source IDs
    val twoBeat = qAP({ (t: TLBundleA, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) => t.size.litValue() == 4 })
    val sourceZero = qAP({ (t: TLBundleA, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) => t.source.litValue() == 0 })
    val sourceOne = qAP({ (t: TLBundleA, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) => t.source.litValue() == 1 })
    val seqZeroProp = qProp[TLBundleA, Int, UInt]((twoBeat & sourceZero) + Implies + ###(1, -1) + (twoBeat & sourceZero))
    val seqOneProp = qProp[TLBundleA, Int, UInt]((twoBeat & sourceOne) + Implies + ###(1, -1) + (twoBeat & sourceOne))

    val putZero = new TLBundleA(params).Lit(_.opcode -> TLOpcodes.PutFullData.U, _.param -> 0.U, _.size -> 4.U,
      _.source -> 0.U, _.address -> 0x8.U, _.mask -> 0xff.U, _.corrupt -> 0.B, _.data -> 0.U)
    val putOne = new TLBundleA(params).Lit(_.opcode -> TLOpcodes.PutFullData.U, _.param -> 0.U, _.size -> 4.U,
      _.source -> 1.U, _.address -> 0x8.U, _.mask -> 0xff.U, _.corrupt -> 0.B, _.data -> 0.U)


    // Sequence with well-formed, consecutive burst transactions
    val inputGoodOne = Seq(putZero, putZero, putOne, putOne)
    assert(seqZeroProp.check(inputGoodOne))
    assert(seqOneProp.check(inputGoodOne))
    seqOneProp.getCoverage()


    // Sequence with well-formed, non-consecutive burst transactions
    val inputGoodTwo = Seq(putZero, putOne, putZero, putOne)
    assert(seqZeroProp.check(inputGoodTwo))
    assert(seqOneProp.check(inputGoodTwo))

    // Sequence with mal-formed burst transactions (missing beat)
    seqZeroProp.clearCoverage()
    val inputBad = Seq(putZero, putOne, putOne)
    println("Below should fail:")
    assert(!seqZeroProp.check(inputBad))
    println("Done.")
    assert(seqOneProp.check(inputBad))
    seqZeroProp.getCoverage()
  }

  it should "sanity test Get -> AccessAckData handshake" in {
    val getTxn = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
      t match {case t: TLBundleA => t.opcode.litValue() == TLOpcodes.Get; case _ => false}}, "If Get transaction")
    val aADTxn = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
      t match {case t: TLBundleD => t.opcode.litValue() == TLOpcodes.AccessAckData; case _ => false}}, "If Access Ack Data transaction")
    val getAADProp = qProp[TLChannel, Int, UInt](getTxn + Implies + ###(1,-1) + aADTxn)

    val inputGood = Seq(Get(0x0), AccessAckData(0x0, 0),
      Get(0x0), AccessAckData(0x0, 0),
      Get(0x0), AccessAckData(0x0, 0))
    assert(getAADProp.check(inputGood))

    val inputBadOne = Seq(Get(0x0))
    assert(!getAADProp.check(inputBadOne))

    // Fails, but missing AccessAckData for third Get (instead of second)
    val inputBadTwo = Seq(Get(0x0), AccessAckData(0x0, 0),
      Get(0x0),
      Get(0x0), AccessAckData(0x0, 0))
    println("Below should fail:")
    assert(!getAADProp.check(inputBadTwo))
    println("Done.")
  }

  it should "sanity test Implications" in {
    val getTxnOp = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
      t match {case t: TLBundleA => t.opcode.litValue() == TLOpcodes.Get; case _ => false}})
    val aADTxnOp = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
      t match {case t: TLBundleD => t.opcode.litValue() == TLOpcodes.AccessAckData; case _ => false}})
    val getAADNoImpProp= qProp[TLChannel, Int, UInt](getTxnOp + ###(1,-1) + aADTxnOp)
    // Random sequence just to test non-triggered property
    val testImplicationProp = qProp[TLChannel, Int, UInt](getTxnOp + ###(1,-1) + aADTxnOp + Implies + aADTxnOp)

    val inputBad = Seq(Get(0x0))
    // Should pass since no implication
    assert(getAADNoImpProp.check(inputBad))
    // Should pass since implication is never met
    assert(testImplicationProp.check(inputBad))
  }

  it should "test Sequence Operations" in {
    val getTxnOp = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
      t match {case t: TLBundleA => t.opcode.litValue() == TLOpcodes.Get; case _ => false}})
    val aADTxnOp = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
      t match {case t: TLBundleD => t.opcode.litValue() == TLOpcodes.AccessAckData; case _ => false}})
    val sourceZero = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
      t match {case t: TLBundleA => t.source.litValue() == 0; case t: TLBundleD => t.source.litValue() == 0}})
    val sourceOne = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
      t match {case t: TLBundleA => t.source.litValue() == 1; case t: TLBundleD => t.source.litValue() == 1}})

    // Source 1 is incomplete
    val input = Seq(Get(0x0), AccessAckData(0x0, 0),
      Get(0x0, source = 1))
    val goodInput = Seq(Get(0x0), AccessAckData(0x0, 0),
      Get(0x0, source = 1), AccessAckData(0x0, 1))
    val goodInput2 = Seq(Get(0x0), AccessAckData(0x0, 0),
      Get(0x0, source = 1), AccessAckData(0x0, 1),
      Get(0x0, source = 1), AccessAckData(0x0, 1))

    var testSeqZero = new Sequence[TLChannel, Int, UInt]()
    testSeqZero += getTxnOp
    testSeqZero += sourceZero
    testSeqZero += Implies
    testSeqZero += ###(1,-1)
    testSeqZero += aADTxnOp
    testSeqZero += sourceZero
    val zeroProp = qProp[TLChannel, Int, UInt](testSeqZero)

    var testSeqOne = new Sequence[TLChannel, Int, UInt]()
    testSeqOne += getTxnOp
    testSeqOne += sourceOne
    testSeqOne += Implies
    testSeqOne += ###(1,-1)
    testSeqOne += aADTxnOp
    testSeqOne += sourceOne
    val oneProp = qProp[TLChannel, Int, UInt](testSeqOne)

    val testSeqCombined = (testSeqZero + ###(1, -1)) + testSeqOne
    val combProp = qProp[TLChannel, Int, UInt](testSeqCombined)

    // Should pass as source 0 has complete req-resp
    assert(zeroProp.check(input))
    println("Below should fail:")
    // Should fail as source 1 has missing response
    assert(!oneProp.check(input))
    // Should fail as source 1 has missing response
    assert(!combProp.check(input))
    println("Done.")
    // Should pass as source 1 has complete req-resp
    assert(oneProp.check(goodInput))
    assert(combProp.check(goodInput))

    val testSeqCombined2 = (testSeqZero + ###(1, -1)) + ((testSeqOne + ###(1, -1)) * 2)
    val combProp2 = qProp[TLChannel, Int, UInt](testSeqCombined2)
    // Should fail as there is only one source 1 transaction
    println("Below should fail:")
    assert(!combProp2.check(goodInput))
    println("Done.")
    // Should pass as there are now 2 source 1 transactions
    assert(combProp2.check(goodInput2))
  }

  it should "sanity test beat checking in bursts with local variables" in {
    // Currently hardcoded for different source IDs
    val twoBeatFirst = qAP({(t: TLBundleA, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
      h("source") = t.source.litValue().toInt; t.size.litValue() == 4})
    val twoBeatLast = qAP({(t: TLBundleA, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
      t.source.litValue() == h("source") & t.size.litValue() == 4})
    val twoBeatProp = qProp[TLBundleA, Int, UInt](twoBeatFirst + Implies + ###(1,-1) + twoBeatLast)

    val putSrcZero = new TLBundleA(params).Lit(_.opcode -> TLOpcodes.PutFullData.U, _.param -> 0.U, _.size -> 4.U,
      _.source -> 0.U, _.address -> 0x8.U, _.mask -> 0xff.U, _.corrupt -> 0.B, _.data -> 0.U)
    val putSrcOne = new TLBundleA(params).Lit(_.opcode -> TLOpcodes.PutFullData.U, _.param -> 0.U, _.size -> 4.U,
      _.source -> 1.U, _.address -> 0x8.U, _.mask -> 0xff.U, _.corrupt -> 0.B, _.data -> 0.U)


    // Sequence with well-formed, consecutive burst transactions
    val goodInput = Seq(putSrcZero, putSrcZero, putSrcOne, putSrcOne)
    assert(twoBeatProp.check(goodInput))

    // Sequence with well-formed, non-consecutive burst transactions
    val anotherGoodInput = Seq(putSrcZero, putSrcOne, putSrcZero, putSrcOne)
    assert(twoBeatProp.check(anotherGoodInput))

    // Sequence with mal-formed burst transactions (missing beat)
    val badInput = Seq(putSrcZero, putSrcOne, putSrcOne)
    println("Below should fail:")
    assert(!twoBeatProp.check(badInput))
    println("Done.")
  }
}