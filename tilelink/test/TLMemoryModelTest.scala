package verif

import freechips.rocketchip.tilelink.{TLBundleA, TLBundleD, TLBundleParameters}
import org.scalatest.flatspec.AnyFlatSpec
import verif.TLTransaction._

object TLMemoryModelSequences {
  // Manually crafted stimulus to test the TLMemoryModel slaving function
  // Returns stimulus to poke on A and expected response on D
  def put(implicit p: TLBundleParameters): (Seq[TLBundleA], Seq[TLBundleD]) = {
    require(p.dataBits == 32) // TODO loosen
    (
      Seq(
        Put(0x8, 0x12345678L),
        Get(0x8),
        Put(0x8, 0xffffffffL),
        Get(0x8),
      ),
      Seq(
        AccessAck(),
        AccessAckData(0x12345678L),
        AccessAck(),
        AccessAckData(0xffffffffL)
      )
    )
  }

  def putWithMask(implicit p: TLBundleParameters): (Seq[TLBundleA], Seq[TLBundleD]) = {
    require(p.dataBits == 32) // TODO loosen
    (
      Seq(
        Put(0x8, 0x12345678L, Integer.parseInt("0110", 2), 0),
        Get(0x8),
        Put(0x8, 0xffffffffL, Integer.parseInt("1001", 2), 0),
        Get(0x8),
        Put(0x8, 0x11118888L, Integer.parseInt("0011", 2), 0),
        Get(0x8)
      ),
      Seq(
        AccessAck(),
        AccessAckData(0x00345600L),
        AccessAck(),
        AccessAckData(0xff3456ffL),
        AccessAck(),
        AccessAckData(0xff348888L)
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
        Get(0x14)
      ) ++
      PutBurst(0x10, Seq(0xbbbbbbbbL, 0xccccccccL), Seq(Integer.parseInt("0011", 2), Integer.parseInt("1000", 2)), 0) ++
      Seq(
        Get(0x10), Get(0x14)
      ),
      Seq(
        AccessAck(4, 0),
        AccessAckData(0x01234567L),
        AccessAckData(0x89abcdefL),
        AccessAckData(0xffffffffL),
        AccessAckData(0x11111111L),
        AccessAck(3, 0),
        AccessAckData(0xffffbbbbL),
        AccessAckData(0xcc111111L)
      )
    )
  }

  def getWithMask(implicit p: TLBundleParameters): (Seq[TLBundleA], Seq[TLBundleD]) = {
    require(p.dataBits == 32) // TODO loosen
    (
      Seq(
        Put(0x8, 0x12345678L),
        Get(0x8, 2, Integer.parseInt("1111", 2), 0),
        Get(0x8, 2, Integer.parseInt("1000", 2), 0),
        Get(0x8, 2, Integer.parseInt("0011", 2), 0),
        Get(0x8, 2, Integer.parseInt("0100", 2), 0),
      ),
      Seq(
        AccessAck(),
        AccessAckData(0x12345678L),
        AccessAckData(0x12000000L),
        AccessAckData(0x00005678L),
        AccessAckData(0x00340000L)
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
        AccessAck(4, 0),
        AccessAckData(0x01234567L, 4, 0, denied = false),
        AccessAckData(0x89abcdefL, 4, 0, denied = false),
        AccessAckData(0xffffffffL, 4, 0, denied = false),
        AccessAckData(0x11111111L, 4, 0, denied = false),
      )
    )
  }

  def logic(implicit p: TLBundleParameters): (Seq[TLBundleA], Seq[TLBundleD]) = {
    require(p.dataBits == 32) // TODO loosen
    val initialData = 0x12345678L
    val newData = 0xabfd2343L
    val xored = initialData ^ newData
    val ored = initialData | newData
    val anded = initialData & newData
    val swapped = newData
    (
      PutBurst(0x0, Seq.fill(4)(initialData), 0) ++
      Seq(
        Logic(TLLogicParam.XOR, 0x0, newData),
        Logic(TLLogicParam.OR, 0x4, newData),
        Logic(TLLogicParam.AND, 0x8, newData),
        Logic(TLLogicParam.SWAP, 0xc, newData),
        Get(0x0),
        Get(0x4),
        Get(0x8),
        Get(0xc)
      ),
      Seq(
        AccessAck(4, 0),
        AccessAckData(initialData),
        AccessAckData(initialData),
        AccessAckData(initialData),
        AccessAckData(initialData),
        AccessAckData(xored),
        AccessAckData(ored),
        AccessAckData(anded),
        AccessAckData(swapped)
      )
    )
  }

  def logicBurst(implicit p: TLBundleParameters): (Seq[TLBundleA], Seq[TLBundleD]) = {
    require(p.dataBits == 32) // TODO loosen
    val initialData = 0x12345678L
    val newData = 0xabfd2343L
    val xored = initialData ^ newData
    (
      PutBurst(0x0, Seq.fill(4)(initialData), 0) ++
      LogicBurst(TLLogicParam.XOR, 0x0, Seq.fill(4)(newData), 0) :+
      Get(0x0, 4, Integer.parseInt("1111", 2), 0)
      ,
      Seq(
        AccessAck(4, 0),
        AccessAckData(initialData, 4, 0, denied = false),
        AccessAckData(initialData, 4, 0, denied = false),
        AccessAckData(initialData, 4, 0, denied = false),
        AccessAckData(initialData, 4, 0, denied = false),
        AccessAckData(xored, 4, 0, denied = false),
        AccessAckData(xored, 4, 0, denied = false),
        AccessAckData(xored, 4, 0, denied = false),
        AccessAckData(xored, 4, 0, denied = false)
      )
    )
  }

  def arith(implicit p: TLBundleParameters): (Seq[TLBundleA], Seq[TLBundleD]) = {
    require(p.dataBits == 32) // TODO loosen
    val initialData = 100
    val newData = 99
    (
      Seq(
        Put(0x0, initialData), // TODO: test all the arithmetic ops
        Arith(TLArithParam.MIN, 0x0, newData),
        Get(0x0)
      ),
      Seq(
        AccessAck(),
        AccessAckData(100),
        AccessAckData(99)
      )
    )
  }

  def arithBurst(implicit p: TLBundleParameters): (Seq[TLBundleA], Seq[TLBundleD]) = {
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
  val bundleParams: TLBundleParameters = TLBundleParameters(32, 32, 2, 1, 4, Seq(), Seq(), Seq(), hasBCE = false)

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

  behavior of "TLMemoryModel"
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
  it should "logic" in {
    test(() => TLMemoryModelSequences.logic(bundleParams))
  }
  it should "logic burst" in {
    test(() => TLMemoryModelSequences.logicBurst(bundleParams))
  }
  it should "arithmetic" in {
    test(() => TLMemoryModelSequences.arith(bundleParams))
  }
}
