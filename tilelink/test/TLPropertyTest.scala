package verif

import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import TLTransaction._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.subsystem.WithoutTLMonitors
import freechips.rocketchip.tilelink.{TLBundleA, TLBundleD, TLBundleParameters, TLChannel}
import org.scalatest.flatspec.AnyFlatSpec

class TLPropertyTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = new WithoutTLMonitors
  implicit val params: TLBundleParameters = TLUtils.defaultVerifTLBundleParams

  it should "test AtmProp and Seq and same cycle checking" in {
    val getAP = new AtmProp[TLBundleA]({t: TLBundleA => t.opcode.litValue() == TLOpcodes.Get}, "If is Get request")
    val sameCycle = new TimeOp(0)
    val paramZero = new AtmProp[TLBundleA]({t: TLBundleA => t.param.litValue() == 0}, "Parameter must be zero")
    val getParamSeq = new Sequence[TLBundleA](getAP, sameCycle, paramZero)
    val getParamProp = new Property[TLBundleA](getParamSeq)

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

  it should "test beat checking in bursts" in {
    // Currently hardcoded for different source IDs
    val twoBeatSourceZero = new AtmProp[TLBundleA]({t: TLBundleA => t.size.litValue().toInt == 4 && t.source.litValue().toInt == 0},
      "If 2 beat burst and source 0")
    val twoBeatSourceOne = new AtmProp[TLBundleA]({t: TLBundleA => t.size.litValue().toInt == 4 && t.source.litValue().toInt == 1},
      "If 2 beat burst and source 1")
    val atLeastOneCycle = new TimeOp(1, modifier = 1)
    val seqZero = new Sequence[TLBundleA](twoBeatSourceZero, atLeastOneCycle, twoBeatSourceZero)
    val seqOne = new Sequence[TLBundleA](twoBeatSourceOne, atLeastOneCycle, twoBeatSourceOne)
    val seqZeroProp = new Property[TLBundleA](seqZero)
    val seqOneProp = new Property[TLBundleA](seqOne)

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

  it should "test Get -> AccessData handshake" in {
    val getTxn = new AtmProp[TLChannel]({case t: TLBundleA => t.opcode.litValue() == TLOpcodes.Get; case _ => false},
      "If Get transaction")
    val aADTxn = new AtmProp[TLChannel]({case t: TLBundleD => t.opcode.litValue() == TLOpcodes.AccessAckData; case _ => false},
      "If Access Ack Data transaction")
    val atLeastOneCycle = new TimeOp(1, modifier = 1)
    val seqGetAAD = new Sequence[TLChannel](getTxn, atLeastOneCycle, aADTxn)
    val getAADProp = new Property[TLChannel](seqGetAAD)

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
}