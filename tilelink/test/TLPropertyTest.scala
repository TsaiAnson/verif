package verif

import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import TLTransaction._
import PSL._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.subsystem.WithoutTLMonitors
import freechips.rocketchip.tilelink.{TLBundleA, TLBundleD, TLBundleParameters, TLChannel}
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.HashMap

class TLPropertyTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = new WithoutTLMonitors
  implicit val params: TLBundleParameters = TLUtils.defaultVerifTLBundleParams

  it should "sanity test AtmProp and Seq and same cycle checking" in {
    val getAP = qAP({(t: TLBundleA, h: HashMap[String, Int]) => t.opcode.litValue() == TLOpcodes.Get}, "If is Get request")
    val paramZero = qAP({(t: TLBundleA, h: HashMap[String, Int]) => t.param.litValue() == 0}, "Parameter must be zero")
    val getParamProp = qProp[TLBundleA, Int](getAP, Implies, ###(0), paramZero)

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

    // One Faulty Get Transaction
    val badGet = new TLBundleA(params).Lit(_.opcode -> TLOpcodes.Get.U, _.param -> 1.U, _.size -> 3.U,
      _.source -> 0.U, _.address -> 0x8.U, _.mask -> 0xff.U, _.corrupt -> 0.B, _.data -> 0.U)
    val inputTransactionsBad = Seq(
      Get(0x0),
      badGet,
      Get(0x10),
      Get(0x18)
    )
    assert(!getParamProp.check(inputTransactionsBad))
  }

  it should "sanity test beat checking in bursts" in {
    // Currently hardcoded for different source IDs
    val twoBeat = qAP({(t: TLBundleA, h: HashMap[String, Int]) => t.size.litValue() == 4}, "If 2 beat burst")
    val sourceZero = qAP({(t: TLBundleA, h: HashMap[String, Int]) => t.source.litValue() == 0}, "If source 0")
    val sourceOne = qAP({(t: TLBundleA, h: HashMap[String, Int]) => t.source.litValue() == 1}, "If source 1")
    val seqZeroProp = qProp[TLBundleA, Int](twoBeat & sourceZero, Implies, ###(1,-1), twoBeat & sourceZero)
    val seqOneProp = qProp[TLBundleA, Int](twoBeat & sourceOne, Implies, ###(1,-1), twoBeat & sourceOne)

    val putZero = new TLBundleA(params).Lit(_.opcode -> TLOpcodes.PutFullData.U, _.param -> 0.U, _.size -> 4.U,
      _.source -> 0.U, _.address -> 0x8.U, _.mask -> 0xff.U, _.corrupt -> 0.B, _.data -> 0.U)
    val putOne = new TLBundleA(params).Lit(_.opcode -> TLOpcodes.PutFullData.U, _.param -> 0.U, _.size -> 4.U,
      _.source -> 1.U, _.address -> 0x8.U, _.mask -> 0xff.U, _.corrupt -> 0.B, _.data -> 0.U)


    // Sequence with well-formed, consecutive burst transactions
    val inputGoodOne = Seq(putZero, putZero, putOne, putOne)
    assert(seqZeroProp.check(inputGoodOne))
    assert(seqOneProp.check(inputGoodOne))

    // Sequence with well-formed, non-consecutive burst transactions
    val inputGoodTwo = Seq(putZero, putOne, putZero, putOne)
    assert(seqZeroProp.check(inputGoodTwo))
    assert(seqOneProp.check(inputGoodTwo))

    // Sequence with mal-formed burst transactions (missing beat)
    val inputBad = Seq(putZero, putOne, putOne)
    assert(!seqZeroProp.check(inputBad))
    assert(seqOneProp.check(inputBad))
  }

  it should "sanity test Get -> AccessAckData handshake" in {
    val getTxn = qAP({(t: TLChannel, h: HashMap[String, Int]) => t match {case t: TLBundleA => t.opcode.litValue() == TLOpcodes.Get; case _ => false}},
      "If Get transaction")
    val aADTxn = qAP({(t: TLChannel, h: HashMap[String, Int]) => t match {case t: TLBundleD => t.opcode.litValue() == TLOpcodes.AccessAckData; case _ => false}},
      "If Access Ack Data transaction")
    val getAADProp = qProp[TLChannel, Int](getTxn, Implies, ###(1,-1), aADTxn)

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
    assert(!getAADProp.check(inputBadTwo))
  }

  it should "sanity test Implications" in {
    val getTxn = qAP({(t: TLChannel, h: HashMap[String, Int]) => t match {case t: TLBundleA => t.opcode.litValue() == TLOpcodes.Get; case _ => false}},
      "If Get transaction")
    val aADTxn = qAP({(t: TLChannel, h: HashMap[String, Int]) => t match {case t: TLBundleD => t.opcode.litValue() == TLOpcodes.AccessAckData; case _ => false}},
      "If Access Ack Data transaction")
    val getAADNoImpProp= qProp[TLChannel, Int](getTxn, ###(1,-1), aADTxn)
    // Random sequence just to test non-triggered property
    val testImplicationProp = qProp[TLChannel, Int](getTxn, ###(1,-1), aADTxn, Implies, aADTxn)

    val inputBad = Seq(Get(0x0))
    // Should pass since no implication
    assert(getAADNoImpProp.check(inputBad))
    // Should pass since implication is never met
    assert(testImplicationProp.check(inputBad))
  }

  it should "test Sequence Operations" in {
    val getTxn = qAP({(t: TLChannel, h: HashMap[String, Int]) => t match {case t: TLBundleA => t.opcode.litValue() == TLOpcodes.Get; case _ => false}},
      "If Get transaction")
    val aADTxn = qAP({(t: TLChannel, h: HashMap[String, Int]) => t match {case t: TLBundleD => t.opcode.litValue() == TLOpcodes.AccessAckData; case _ => false}},
      "If Access Ack Data transaction")
    val sourceZero = qAP({(t: TLChannel, h: HashMap[String, Int]) => t match {case t: TLBundleA => t.source.litValue() == 0; case t: TLBundleD => t.source.litValue() == 0}},
      "If source 0")
    val sourceOne = qAP({(t: TLChannel, h: HashMap[String, Int]) => t match {case t: TLBundleA => t.source.litValue() == 1; case t: TLBundleD => t.source.litValue() == 1}},
      "If source 1")

    // Source 1 is incomplete
    val input = Seq(Get(0x0), AccessAckData(0x0, 0),
      Get(0x0, source = 1))

    var testSeqZero = new Sequence[TLChannel, Int]()
    println("Following should have an error:")
    testSeqZero += ###(0)
    println("Done")
    println("Following should have an error:")
    testSeqZero += Implies
    println("Done")
    println("Following should not have an error but exactly 2 warnings:")
    testSeqZero += getTxn
    testSeqZero += sourceZero
    testSeqZero += Implies
    testSeqZero += ###(1,-1)
    testSeqZero += aADTxn
    testSeqZero += sourceZero
    println("Done")
    val zeroProp = new Property[TLChannel, Int](testSeqZero)

    var testSeqOne = new Sequence[TLChannel, Int]()
    println("Following should not have an error but exactly 2 warnings:")
    testSeqOne += getTxn
    testSeqOne += sourceOne
    testSeqOne += Implies
    testSeqOne += ###(1,-1)
    testSeqOne += aADTxn
    testSeqOne += sourceOne
    println("Done")
    val oneProp = new Property[TLChannel, Int](testSeqOne)

    val testSeqCombined = testSeqZero + testSeqOne
    val combProp = new Property[TLChannel, Int](testSeqCombined)

    // Should pass as source 0 has complete req-resp
    assert(zeroProp.check(input))
    // Should fail as source 1 has missing response
    assert(!oneProp.check(input))
    // Should fail as source 1 has missing response
    assert(!combProp.check(input))
  }

  it should "sanity test beat checking in bursts with local variables" in {
    // Currently hardcoded for different source IDs
    val twoBeatFirst = qAP({(t: TLBundleA, h: HashMap[String, Int]) => h("source") = t.source.litValue().toInt; t.size.litValue() == 4}, "If 2 beat burst start")
    val twoBeatLast = qAP({(t: TLBundleA, h: HashMap[String, Int]) => t.source.litValue() == h("source"); t.size.litValue() == 4}, "If 2 beat burst end")
    val twoBeatProp = qProp[TLBundleA, Int](twoBeatFirst, Implies, ###(1,-1), twoBeatLast)

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
    assert(!twoBeatProp.check(badInput))
  }
}