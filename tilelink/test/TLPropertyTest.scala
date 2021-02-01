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

  it should "test self property" in {
    def zeroParamGet(t: TLBundleA): Boolean = {
      if (t.opcode.litValue() == TLOpcodes.Get) {
        t.param.litValue() == 0
      } else {
        true
      }
    }
    val getProperty = TLSelfPropertyA(zeroParamGet)

    val inputTransactions = Seq(
      // Read back the values in registers 0x00, 0x08, 0x10, 0x18
      Get(0x0),
      Get(0x08),
      Get(0x10),
      Get(0x18),
      // Write values into registers 0x00, 0x08, 0x10, 0x18
      Put(0x0, 0),
      Put(0x8, 1),
      Put(0x10, 2),
      Put(0x18, 3),
      // Read back the values in registers 0x00, 0x08, 0x10, 0x18
      Get(0x0),
      Get(0x08),
      Get(0x10),
      Get(0x18)
    )
    assert(getProperty.check(inputTransactions).foldLeft(true)(_ & _))

    val inputTransactionsBad = Seq(
      // Read back the values in registers 0x00, 0x08, 0x10, 0x18
      Get(0x0),
      Get(0x08),
      Get(0x10),
      new TLBundleA(params).Lit(_.opcode -> TLOpcodes.Get.U, _.param -> 1.U, _.size -> 3.U,
        _.source -> 0.U, _.address -> 0x0.U, _.mask -> 0xff.U, _.corrupt -> 0.B, _.data -> 0.U),
      // Write values into registers 0x00, 0x08, 0x10, 0x18
      Put(0x0, 0),
      Put(0x8, 1),
      Put(0x10, 2),
      Put(0x18, 3),
      // Read back the values in registers 0x00, 0x08, 0x10, 0x18
      Get(0x0),
      Get(0x08),
      Get(0x10),
      Get(0x18)
    )
    assert(!getProperty.check(inputTransactionsBad).foldLeft(true)(_ & _))
  }

  it should "test future property in bursts" in {
    def twoBeatBurst(t: TLBundleA): Boolean = {
      t.size.litValue() == 4
    }
    val twoBeatBurstProperty = TLFuturePropertyA(twoBeatBurst, twoBeatBurst, 1)

    val input = PutBurst(0x0, Seq(0x1234, 0x5678), 0) ++
      Seq(Get(0x0),
        Get(0x08),
        Put(0x0, 0)) ++
      PutBurst(0x0, Seq(0x1234, 0x5678), 0) ++
      PutBurst(0x0, Seq(0x1234, 0x5678), 0) ++
      Seq(Get(0x0),
        Get(0x08),
        Put(0x0, 0)) ++
      PutBurst(0x0, Seq(0x1234, 0x5678), 0)
    assert(twoBeatBurstProperty.check(input))

    val inputBad = PutBurst(0x0, Seq(0x1234, 0x5678), 0) ++
      Seq(Get(0x0),
        Get(0x08),
        Put(0x0, 0)) ++
      PutBurst(0x0, Seq(0x1234, 0x5678), 0) ++
      Seq(new TLBundleA(params).Lit(_.opcode -> TLOpcodes.PutFullData.U, _.param -> 0.U, _.size -> 4.U,
        _.source -> 0.U, _.address -> 0x0.U, _.mask -> 0xff.U, _.corrupt -> 0.B, _.data -> 0.U)) ++
      Seq(Get(0x0),
        Get(0x08),
        Put(0x0, 0)) ++
      PutBurst(0x0, Seq(0x1234, 0x5678), 0)
    assert(!twoBeatBurstProperty.check(inputBad))
  }

  it should "test future property in Get -> AccessAckData transaction (single beat, no concurrent txns)" in {
    def getReq(t: TLChannel): Boolean = {
      t match {
        case t: TLBundleA => t.opcode.litValue() == TLOpcodes.Get
        case _ => false
      }
    }
    def accessAckDataResp(t: TLChannel): Boolean = {
      t match {
        case t: TLBundleD => t.opcode.litValue() == TLOpcodes.AccessAckData
        case _ => false
      }
    }
    val getTxnProperty = TLFuturePropertyAD(getReq, accessAckDataResp)

    val input = Seq(
      Get(0x0),
      AccessAckData(0x1234, 0),
      Get(0x1),
      AccessAckData(0x5678, 0)
    )
    assert(getTxnProperty.check(input))

    val inputBadOne = Seq(
      Get(0x0),
      AccessAckData(0x1234, 0),
      Get(0x1)
    )
    assert(!getTxnProperty.check(inputBadOne))

    val inputBadTwo = Seq(
      Get(0x0),
      AccessAckData(0x1234, 0),
      Get(0x1),
      Get(0x2),
      AccessAckData(0x1234, 0)
    )
    // TODO This one "passes" the check since the third Get gets skipped over
    // Ways to get around this: Add checks for matching source ID (as sourceID is unique across unique transactions)
    assert(!getTxnProperty.check(inputBadTwo))
  }
}