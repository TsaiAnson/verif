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

class TLSLMemoryModelTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val params: TLBundleParameters = TLBundleParameters(DefaultTLParams.master(), DefaultTLParams.slave)

  it should "single source memory request (no burst)" in {
    val input = Seq(
      Put(0x0, 0x1234),
      Get(0x0),
      AccessAckData(0x1234, 0)
    )

    val mm = new SLTLMemoryModel(params)
    val result = mm.model(input)

    val expected = Seq(None, None, Some(new SLOptTLMemoryState(0x1234.U)))
    assert(result == expected)
  }

  it should "single source memory request (burst)" in {
    val input = PutBurst(0x0, Seq(0x1234, 0x5678), 0) ++
      Seq(
        Get(0x0, 4, 0xff, 0),
        AccessAckData(0x1234, 0),
        AccessAckData(0x5678, 0)
      )

    val mm = new SLTLMemoryModel(params)
    val result = mm.model(input)

    val expected = Seq(None, None, None, Some(new SLOptTLMemoryState(0x1234.U)), Some(new SLOptTLMemoryState(0x5678.U)))
    assert(result == expected)
  }

  it should "multi source memory requests (no burst)" in {
    val input = Seq(
      Put(0x0, 0x1234),
      Put(0x8, 0x8888, 1),
      Get(0x0),
      Get(0x8, 1),
      AccessAckData(0x8888, 1),
      AccessAckData(0x1234, 0)
    )

    val mm = new SLTLMemoryModel(params)
    val result = mm.model(input)

    val expected = Seq(None, None, None, None,
      Some(new SLOptTLMemoryState(0x8888.U)), Some(new SLOptTLMemoryState(0x1234.U)))
    assert(result == expected)
  }

  it should "multi source memory requests (burst, sequential)" in {
    val input = PutBurst(0x0, Seq(0x1234, 0x5678), 0) ++
      PutBurst(0x10, Seq(0x2222, 0x6666), 0) ++ // Note that the put source can the same
      Seq(Get(0x0, 4, 0xff, 0)) ++
      AccessAckDataBurst(Seq(0x1234, 0x5678), 0) ++
      Seq(Get(0x10, 4, 0xff, 1)) ++
      AccessAckDataBurst(Seq(0x2222, 0x6666), 1)

    val mm = new SLTLMemoryModel(params)
    val result = mm.model(input)

    val expected = Seq(None, None, None, None, None, Some(new SLOptTLMemoryState(0x1234.U)), Some(new SLOptTLMemoryState(0x5678.U)),
      None, Some(new SLOptTLMemoryState(0x2222.U)), Some(new SLOptTLMemoryState(0x6666.U)))
    assert(result == expected)
  }

  it should "multi source memory requests (burst, non-sequential)" in {
    val input = PutBurst(0x0, Seq(0x1234, 0x5678), 0) ++
      PutBurst(0x10, Seq(0x2222, 0x6666), 0) ++ // Note that the put source can the same
      Seq(
        Get(0x0, 4, 0xff, 0),
        Get(0x10, 4, 0xff, 1),
        AccessAckData(0x1234, 4, 0, false),
        AccessAckData(0x2222, 4, 1, false),
        AccessAckData(0x6666, 4, 1, false),
        AccessAckData(0x5678, 4, 0, false)
    )

    val mm = new SLTLMemoryModel(params)
    val result = mm.model(input)

    val expected = Seq(None, None, None, None, None, None,
      Some(new SLOptTLMemoryState(0x1234.U)), Some(new SLOptTLMemoryState(0x2222.U)),
      Some(new SLOptTLMemoryState(0x6666.U)), Some(new SLOptTLMemoryState(0x5678.U)))
    assert(result == expected)
  }

  it should "test expected data in get->ackdata transactions" in {
    val getTxn = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
      t match {
        case t: TLBundleA =>
          h("src") = t.source.litValue().toInt
          t.size.litValue() == 4 && t.opcode.litValue() == TLOpcodes.Get
        case _ => false
      }}, "If Get transaction")
    val aADTxn = qAP({(t: TLChannel, h: HashMap[String, Int], m: Option[SLMemoryState[UInt]]) =>
      t match {
        case t: TLBundleD =>
          t.size.litValue() == 4 && t.source.litValue().toInt == h("src") && t.opcode.litValue() == TLOpcodes.AccessAckData &&
            t.data.litValue() == m.get.get(0).litValue()
        case _ => false
      }}, "If Access Ack Data transaction")
    val dataTwoBeatProp = qProp[TLChannel, Int, UInt](getTxn, Implies, ###(1,-1), aADTxn, ###(1,-1), aADTxn)

    val inputGood = PutBurst(0x0, Seq(0x1234, 0x5678), 0) ++
      PutBurst(0x10, Seq(0x2222, 0x6666), 0) ++ // Note that the put source can the same
      Seq(
        Get(0x0, 4, 0xff, 0),
        Get(0x10, 4, 0xff, 1),
        AccessAckData(0x1234, 4, 0, false),
        AccessAckData(0x2222, 4, 1, false),
        AccessAckData(0x6666, 4, 1, false),
        AccessAckData(0x5678, 4, 0, false)
      )
    val mm = new SLTLMemoryModel(params)
    val result = mm.model(inputGood)
    assert(dataTwoBeatProp.check(inputGood, result))

    val inputBad = PutBurst(0x0, Seq(0x1234, 0x5678), 0) ++
      PutBurst(0x10, Seq(0x2222, 0x6666), 0) ++ // Note that the put source can the same
      Seq(
        Get(0x0, 4, 0xff, 0),
        Get(0x10, 4, 0xff, 1),
        AccessAckData(0x1234, 4, 0, false),
        AccessAckData(0x2222, 4, 1, false),
        AccessAckData(0x1234, 4, 1, false), // Transaction  has bad data
        AccessAckData(0x5678, 4, 0, false)
      )
    val resultBad = mm.model(inputBad)
    assert(result == resultBad) // The bad response data should not modify the memory states
    assert(!dataTwoBeatProp.check(inputBad, resultBad))
  }
}
