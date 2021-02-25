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

class PSLMemoryModelTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val params: TLBundleParameters = TLUtils.defaultVerifTLBundleParams

  it should "single source memory request (no burst)" in {
    val input = Seq(
      Put(0x0, 0x1234),
      Get(0x0),
      AccessAckData(0x1234, 0)
    )

    val mm = new PSLTLMemoryModel(params)
    val result = mm.model(input)

    val expected = Seq(None, None, Some(new PSLOptTLMemoryState(0x1234.U)))
    assert(result == expected)
  }

  it should "single source memory request (burst)" in {
    val input = PutBurst(0x0, Seq(0x1234, 0x5678), 0) ++
      Seq(
        Get(0x0, 4, 0xff, 0),
        AccessAckData(0x1234, 0),
        AccessAckData(0x5678, 0)
      )

    val mm = new PSLTLMemoryModel(params)
    val result = mm.model(input)

    val expected = Seq(None, None, None, Some(new PSLOptTLMemoryState(0x1234.U)), Some(new PSLOptTLMemoryState(0x5678.U)))
    assert(result == expected)
  }

  it should "multi source memory requests (no burst)" in {
    val input = Seq(
      Put(0x0, 0x1234),
      Put(0x8, 0x8888, 1),
      Get(0x0),
      Get(0x8, 1),
      AccessAckData(0x8888, 0, 1),
      AccessAckData(0x1234, 0)
    )

    val mm = new PSLTLMemoryModel(params)
    val result = mm.model(input)

    val expected = Seq(None, None, None, None,
      Some(new PSLOptTLMemoryState(0x8888.U)), Some(new PSLOptTLMemoryState(0x1234.U)))
    assert(result == expected)
  }

  it should "multi source memory requests (burst, sequential)" in {
    val input = PutBurst(0x0, Seq(0x1234, 0x5678), 0) ++
      PutBurst(0x10, Seq(0x2222, 0x6666), 0) ++ // Note that the put source can the same
      Seq(
        Get(0x0, 4, 0xff, 0),
        AccessAckData(0x1234, 0),
        AccessAckData(0x5678, 0)
      ) ++
      Seq(
        Get(0x10, 4, 0xff, 1),
        AccessAckData(0x2222, 0, 1),
        AccessAckData(0x6666, 0, 1)
      )

    val mm = new PSLTLMemoryModel(params)
    val result = mm.model(input)

    val expected = Seq(None, None, None, None, None, Some(new PSLOptTLMemoryState(0x1234.U)), Some(new PSLOptTLMemoryState(0x5678.U)),
      None, Some(new PSLOptTLMemoryState(0x2222.U)), Some(new PSLOptTLMemoryState(0x6666.U)))
    assert(result == expected)
  }

  it should "multi source memory requests (burst, non-sequential)" in {
    val input = PutBurst(0x0, Seq(0x1234, 0x5678), 0) ++
      PutBurst(0x10, Seq(0x2222, 0x6666), 0) ++ // Note that the put source can the same
      Seq(
        Get(0x0, 4, 0xff, 0),
        Get(0x10, 4, 0xff, 1),
        AccessAckData(0x1234, 0),
        AccessAckData(0x2222, 0, 1),
        AccessAckData(0x6666, 0, 1),
        AccessAckData(0x5678, 0)
      )

    val mm = new PSLTLMemoryModel(params)
    val result = mm.model(input)

    val expected = Seq(None, None, None, None, None, None,
      Some(new PSLOptTLMemoryState(0x1234.U)), Some(new PSLOptTLMemoryState(0x2222.U)),
      Some(new PSLOptTLMemoryState(0x6666.U)), Some(new PSLOptTLMemoryState(0x5678.U)))
    assert(result == expected)
  }
}