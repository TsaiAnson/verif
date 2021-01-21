package verif

import freechips.rocketchip.tilelink.{TLBundleA, TLBundleD, TLBundleParameters}
import org.scalatest.flatspec.AnyFlatSpec
import verif.TLTransaction._

object TLMemoryModelSequences {
  // Manually crafted stimulus to test the TLMemoryModel slaving function
  def put(implicit p: TLBundleParameters): (Seq[TLBundleA], Seq[TLBundleD]) = {
    require(p.dataBits == 32) // TODO loosen
    (
      Seq(
        Put(0x8, 0x12345678L),
        Get(0x8),
        Put(0x8, 0xffffffffL, Integer.parseInt("1111", 2)),
        Get(0x8),
      ),
      Seq(
        AccessAck(0),
        AccessAckData(0x12345678L, 0),
        AccessAck(0),
        AccessAckData(0xffffffffL, 0)
      )
    )
  }

  def putWithMask(implicit p: TLBundleParameters): (Seq[TLBundleA], Seq[TLBundleD]) = {
    require(p.dataBits == 32) // TODO loosen
    (
      Seq(
        Put(0x8, 0x12345678L, Integer.parseInt("0110", 2)),
        Get(0x8),
        Put(0x8, 0xffffffffL, Integer.parseInt("1001", 2)),
        Get(0x8),
        Put(0x8, 0x11118888L, Integer.parseInt("0011", 2)),
        Get(0x8)
      ),
      Seq(
        AccessAck(0),
        AccessAckData(0x00345600L, 0),
        AccessAck(0),
        AccessAckData(0xff3456ffL, 0),
        AccessAck(0),
        AccessAckData(0xff348888L, 0)
      )
    )
  }

  def putBurst(implicit p: TLBundleParameters): (Seq[TLBundleA], Seq[TLBundleD]) = {
    require(p.dataBits == 32) // TODO loosen
    (
      PutBurst(0x8, Seq(0x01234567L, 0x89abcdefL, 0xffffffffL, 0x11111111L), 0) ++
      Seq(
        Get(0x8),
        Get(0xc),
        Get(0x10),
        Get(0x14)// TODO: add a PutBurst with mask too
      ) ++
      PutBurst(0x10, Seq(0xbbbbbbbbL, 0xccccccccL), Seq(Integer.parseInt("0011", 2), Integer.parseInt("1000", 2)), 0) ++
      Seq(
        Get(0x10), Get(0x14)
      ),
      Seq(
        AccessAck(0, 4, 0),
        AccessAckData(0x01234567L, 0),
        AccessAckData(0x89abcdefL, 0),
        AccessAckData(0xffffffffL, 0),
        AccessAckData(0x11111111L, 0),
        AccessAck(0, 3, 0),
        AccessAckData(0xffffbbbbL, 0, 0),
        AccessAckData(0xcc111111L, 0, 0)
      )
    )
  }

  def getWithMask(implicit p: TLBundleParameters): (Seq[TLBundleA], Seq[TLBundleD]) = {
    require(p.dataBits == 32) // TODO loosen
    (
      Seq(
        Put(0x8, 0x12345678L),
        Get(0x8, 2, Integer.parseInt("1111", 2), source = 0),
        Get(0x8, 2, Integer.parseInt("1000", 2), 0),
        Get(0x8, 2, Integer.parseInt("0011", 2), 0),
        Get(0x8, 2, Integer.parseInt("0100", 2), 0),
      ),
      Seq(
        AccessAck(0),
        AccessAckData(0x12345678L, 0),
        AccessAckData(0x12000000L, 0),
        AccessAckData(0x00005678L, 0),
        AccessAckData(0x00340000L, 0),
      )
    )
  }

  def getBurst(implicit p: TLBundleParameters): (Seq[TLBundleA], Seq[TLBundleD]) = {
    require(p.dataBits == 32) // TODO loosen
    (
      PutBurst(0x8, Seq(0x01234567L, 0x89abcdefL, 0xffffffffL, 0x11111111L), 0) ++
      Seq(Get(0x8, 4, Integer.parseInt("1111", 2), 0))
    ,
      Seq(
        AccessAck(0, 4, 0),
        AccessAckData(0x01234567L, 0, 4, 0),
        AccessAckData(0x89abcdefL, 0, 4, 0),
        AccessAckData(0xffffffffL, 0, 4, 0),
        AccessAckData(0x11111111L, 0, 4, 0),
      )
    )
  }

  def logic(implicit p: TLBundleParameters): (Seq[TLBundleA], Seq[TLBundleD]) = {
    ???
  }

  def logicBurstWithMask(implicit p: TLBundleParameters): (Seq[TLBundleA], Seq[TLBundleD]) = {
    ???
  }

  def arith(implicit p: TLBundleParameters): (Seq[TLBundleA], Seq[TLBundleD]) = {
    ???
  }

  def arithBurstWithMask(implicit p: TLBundleParameters): (Seq[TLBundleA], Seq[TLBundleD]) = {
    ???
  }

  def sourcePropagate(implicit p: TLBundleParameters): (Seq[TLBundleA], Seq[TLBundleD]) = {
    ???
  }

  def interleave(implicit p: TLBundleParameters): (Seq[TLBundleA], Seq[TLBundleD]) = {
    ???
  }
}

class TLMemoryModelTest extends AnyFlatSpec {
  implicit val bundleParams: TLBundleParameters = TLBundleParameters(32, 32, 2, 1, 4, Seq(), Seq(), Seq(), hasBCE = false)

  def test(fn: () => (Seq[TLBundleA], Seq[TLBundleD])): Unit= {
    val memoryModel = new TLMemoryModel(bundleParams)
    val (stimulus, expected) = fn()
    val (responseTxns, newState) = memoryModel.respondFromState(stimulus, TLMemoryModel.State.empty())
    val comparison = equalsTL(responseTxns.collect { case t: TLBundleD => t }, expected)
    if (comparison.nonEmpty) {
      println("Expected")
      expected.foreach(println(_))
      println()
      println("Got")
      responseTxns.foreach(println(_))
      println()
      println("Diffs")
      comparison.foreach(println(_))
      assert(comparison.isEmpty)
    }
  }

  behavior of "TLSlaveDriver"
  it should "put basic" in {
    test(() => TLMemoryModelSequences.put(bundleParams))
  }
  it should "put with mask" in {
    test(() => TLMemoryModelSequences.putWithMask(bundleParams))
  }
  it should "put bursts" in {
    test(() => TLMemoryModelSequences.putBurst(bundleParams))
  }
  it should "get with masks" in {
    test(() => TLMemoryModelSequences.getWithMask(bundleParams))
  }
  it should "get with bursts" in {
    test(() => TLMemoryModelSequences.getBurst(bundleParams))
  }
}
