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
import freechips.rocketchip.tilelink.{ReadPattern, TLBundleA, TLBundleD, TLBundleParameters, WritePattern}

class SlaveDriverTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "TLSlaveDriver"
  implicit val p: Parameters = new WithoutTLMonitors

  it should "receive transactions from TLPatternPusher and respond correctly" in {
    val patterns = Seq(WritePattern(0x10, 3, 100), ReadPattern(0x10, 3))
    val pusher = LazyModule(new TLPatternPusherStandalone(patterns))

    test(pusher.module).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      implicit val bundleParams: TLBundleParameters = pusher.out.params
      val slaveModel = new TLDriverSlave(c.clock, pusher.out, SlaveMemoryState.init(), testResponseWrapper)
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
      implicit val params: TLBundleParameters = fuzzer.out.params
      val slave = new TLDriverSlave(c.clock, fuzzer.out, SlaveMemoryState.init(), testResponseWrapper)
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

      val inputTransactions: Seq[TLBundleA] = Seq(
        Get(0x0),
        Put(0x0, 0x3333),
        Get(0x0)
      ) ++
        (PutBurst(0x0, Seq(0x5555, 0x1234), 0) :+
          Get(0x0) :+
          Get(0x8)) ++
        (LogicBurst(2, 0x0, Seq(0x0, 0x0)) :+
          Get(0x0) :+
          Get(0x8)) ++
        ArithBurst(4, 0x0, Seq(0x2222, 0x8888)) :+
        Get(0x0) :+
        Get(0x8)

      mDriver.push(inputTransactions)
      c.clock.step(500)

      val output = monitor.getMonitoredTransactions().map(_.data).collect{case t: TLBundleD => t}

      val expected: Seq[TLBundleD] = Seq(
        AccessAckData(0x0, 0),        // from Get 0x0
        AccessAck(0),                       // from Put 0x0 0x3333
        AccessAckData(0x3333, 0),     // from Get 0x0
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

      //assert(output.length == expected.length)
      output.foreach(t=>println(t.opcode, t.data, t.size))
      output.zip(expected).foreach {
        case (seen, expected) =>
          //println(expected.opcode, expected.data, expected.size)
          //assert(seen.opcode.litValue() == expected.opcode.litValue())
          //assert(seen.denied.litValue() == expected.denied.litValue())
          //assert(seen.data.litValue() == expected.data.litValue())
      }
    }
  }
}
