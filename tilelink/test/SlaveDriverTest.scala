package verif

import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.WithoutTLMonitors
import TLUtils._
import TLTransaction._
import freechips.rocketchip.tilelink.{ReadPattern, TLBundleA, TLBundleD, TLBundleParameters, TLChannel, WritePattern}

object MemoryModelSequences {
  // Manually crafted stimulus to test the TLMemoryModel slaving function
  def put(implicit p: TLBundleParameters): (Seq[TLBundleA], Seq[TLBundleD]) = {
    require(p.dataBits == 32) // TODO loosen
    require(p.addressBits > 8)
    (
      Seq(
        Put(0x8, 0x12345678),
        Get(0x8),
        Put(0x8, 0xffffffffL, Integer.parseInt("1111", 2)),
        Get(0x8),
      ),
      Seq(
        AccessAck(0),
        AccessAckData(0x12345678, 0),
        AccessAck(0),
        AccessAckData(0xffffffffL, 0)
      )
    )
  }

  def putWithMask(implicit p: TLBundleParameters): (Seq[TLBundleA], Seq[TLBundleD]) = {
    ???
  }

  def putBurstWithMask(implicit p: TLBundleParameters): (Seq[TLBundleA], Seq[TLBundleD]) = {
    ???
  }

  def getBurstWithMask(implicit p: TLBundleParameters): (Seq[TLBundleA], Seq[TLBundleD]) = {
    ???
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

class SlaveDriverTest extends AnyFlatSpec with ChiselScalatestTester {
  //implicit val p: Parameters = new WithoutTLMonitors

  case class Mismatch(idx: Int, field: chisel3.Data, expected: chisel3.Data, actual:chisel3.Data) {
    override def toString: String = ???
  }

  def equalsTL(a: Seq[TLBundleD], b: Seq[TLBundleD]): Seq[Mismatch] = {
    ???
  }
  behavior of "TLSlaveDriver"
  implicit val bundleParams: TLBundleParameters = TLBundleParameters(32, 32, 2, 1, 4, Seq(), Seq(), Seq(), hasBCE = false)
  it should "work with put" in {
    val memoryModel = new TLMemoryModel(bundleParams)
    val (stimulus, expected) = MemoryModelSequences.put
    val (responseTxns, newState) = memoryModel.respondFromState(stimulus, TLMemoryModel.State.empty())
    val comparison = equalsTL(responseTxns.collect{ case t: TLBundleD => t}, expected)
    println(comparison)
  }

  /*
  it should "produce the correct slave responses for manual stimulus" in {
    implicit val bundleParams: TLBundleParameters = TLUtils.verifTLBundleParams
    val masterStimulus: Seq[TLBundleA] = Seq(
      Get(0x0),
      Put(0x0, 0x1234),
      Get(0x0)
    )
      (PutBurst(0x0, Seq(0x5555, 0x1234), 0) :+
        Get(0x0) :+
        Get(0x8)) ++
      (LogicBurst(2, 0x0, Seq(0x0, 0x0)) :+
        Get(0x0) :+
        Get(0x8)) ++
      ArithBurst(4, 0x0, Seq(0x2222, 0x8888)) :+
      Get(0x0) :+
      Get(0x8)


    val expectedSlaveResponse: Seq[TLBundleD] = Seq(
      AccessAckData(0x0, 0),        // from Get 0x0
      AccessAck(0),                       // from Put 0x0 0x3333
      AccessAckData(0x1234, 0),     // from Get 0x0
      AccessAck(0, 4, 0),    // from PutBurst 0x0 0x5555, 0x8 0x1234
      AccessAckData(0x5555, 0),     // from Get 0x0
      AccessAckData(0x1234, 0),     // from Get 0x8
      AccessAckData(0x5555, 0),     // from LogicBurst 0x0
      AccessAckData(0x1234, 0),     // from LogicBurst 0x8
      AccessAckData(0x0, 0),        // from Get 0x0
      AccessAckData(0x0, 0),        // from Get 0x8
      AccessAckData(0x0, 0),        // from ArithBurst 0x0
      AccessAckData(0x0, 0),        // from ArithBurst 0x8
      AccessAckData(0x2222, 0),     // from Get 0x0
      AccessAckData(0x8888, 0),     // from Get 0x8
    )

    val slaveFn = new TLMemoryModel(bundleParams, TLMemoryModel.State.empty())
    val (responseTxns, newState) = masterStimulus.foldLeft((Seq.empty[TLChannel], slaveFn.initialState)) {
      case ((responses, state), tx) =>
        val (newTxns, newState) = slaveFn.response(tx, state)
        (responses ++ newTxns, newState)
    }

    responseTxns.map(_.asInstanceOf[TLBundleD]).zip(expectedSlaveResponse).foreach {
      case (respTx, expTx) =>
        assert(respTx.opcode.litValue() == expTx.opcode.litValue())
        assert(respTx.denied.litValue() == expTx.denied.litValue())
        assert(respTx.size.litValue() == expTx.size.litValue())
        assert(respTx.source.litValue() == expTx.source.litValue())
        if (respTx.opcode.litValue().toInt == TLOpcodes.AccessAckData) {
          assert(respTx.data.litValue() == expTx.data.litValue())
        }
    }
    //println(responseTxns)
  }

   */

  /*
  it should "receive transactions from TLPatternPusher and respond correctly" in {
    val patterns = Seq(WritePattern(0x10, 3, 100), ReadPattern(0x10, 3))
    val pusher = LazyModule(new TLPatternPusherStandalone(patterns))

    test(pusher.module).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      implicit val bundleParams: TLBundleParameters = pusher.out.params
      val slaveFn = new TLMemoryModel(bundleParams, TLMemoryModel.State.empty())
      val slaveModel = new TLDriverSlave(c.clock, pusher.out, slaveFn)
      val monitor = new TLMonitor(c.clock, pusher.out)

      c.clock.step(100)

      val monitored = monitor.getMonitoredTransactions().map(_.data)
      val monitoredA = monitored.collect{ case t: TLBundleA => t }
      val monitoredD = monitored.collect{ case t: TLBundleD => t }
      monitoredA.zip(Seq(
        Put(0x0, 100), Get(0x0)
      )).foreach {
        case (seen, expected) =>
          assert(seen.opcode.litValue() == expected.opcode.litValue())
          assert(seen.data.litValue() == expected.data.litValue())
          assert(seen.mask.litValue() == expected.mask.litValue())
      }

      monitoredD.zip(Seq(
        AccessAck(0), AccessAckData(100, 0)
      )).foreach {
        case (seen, expected) =>
          assert(seen.opcode.litValue() == expected.opcode.litValue())
          assert(seen.denied.litValue() == expected.denied.litValue())
          assert(seen.data.litValue() == expected.data.litValue())
      }
    }
  }

  it should "handle transactions from the synthesizable TLFuzzer" in {
    val N = 30
    val fuzzer = LazyModule(new TLFuzzerStandalone(N))
    test(fuzzer.module).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      implicit val bundleParams: TLBundleParameters = fuzzer.out.params
      val slaveFn = new TLMemoryModel(bundleParams, TLMemoryModel.State.empty())
      val slaveModel = new TLDriverSlave(c.clock, fuzzer.out, slaveFn)
      val monitor = new TLMonitor(c.clock, fuzzer.out)

      c.clock.step(100)

      val output = monitor.getMonitoredTransactions()
      // Sanity test to make sure that driver/monitor is correctly getting requests
      assert(output.length == 60)
    }
  }

  it should "handle transactions driven by the TLDriverMaster" in {
    val passthrough = LazyModule(new TLBufferStandalone)
    test(passthrough.module).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      implicit val params: TLBundleParameters = passthrough.in.params

      // Drivers/Monitors
      val mDriver = new TLDriverMaster(c.clock, passthrough.in)
      val sDriver = new TLDriverSlave(c.clock, passthrough.out, SlaveMemoryState.init(), testResponseWrapper)
      val monitor = new TLMonitor(c.clock, passthrough.in)

      mDriver.push(masterStimulus)
      c.clock.step(500)

      val output = monitor.getMonitoredTransactions().map(_.data).collect{case t: TLBundleD => t}

      //assert(output.length == expected.length)
      output.foreach(t=>println(t.opcode, t.data, t.size))
      output.zip(expectedSlaveResponse).foreach {
        case (seen, expected) =>
          //println(expected.opcode, expected.data, expected.size)
          //assert(seen.opcode.litValue() == expected.opcode.litValue())
          //assert(seen.denied.litValue() == expected.denied.litValue())
          //assert(seen.data.litValue() == expected.data.litValue())
      }
    }
  }

   */
}
