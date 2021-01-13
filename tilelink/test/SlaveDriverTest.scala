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
import freechips.rocketchip.tilelink.{ReadPattern, TLBundleA, TLBundleD, TLBundleE, TLBundleParameters, TLDataChannel, WritePattern}

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
    val loopback = LazyModule(new TLLoopbackStandalone)
    test(loopback.module).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      implicit val params = loopback.in.params

      // Drivers/Monitors
      val mDriver = new TLDriverMaster(c.clock, loopback.in)
      val sDriver = new TLDriverSlave(c.clock, loopback.out, SlaveMemoryState.init(), testResponseWrapper)

      val monitor = new TLMonitor(c.clock, loopback.in)

      val simCycles = 500

      val inputTransactions: Seq[TLBundleA] = Seq(
        Get(0x0),
        Put(0x0, 0x3333),
        Get(0x0)
      ) ++
        (PutBurst(0x0, Seq(0x3333, 0x1234), 0) :+
          Get(0x0) :+
          Get(0x8) :+
          Logic(2, 0x0, 0x0)) ++
        (LogicBurst(2, 0x0, Seq(0x0, 0x0)) :+
          Get(0x0) :+
          Get(0x8) :+
          Arith(4, 0x0, 0x0) :+
          Get(0x0)) ++
        ArithBurst(4, 0x0, Seq(0x0, 0x0)) :+
        Get(0x0) :+
        Get(0x8)

      mDriver.push(inputTransactions)
      c.clock.step(simCycles)

      val output = monitor.getMonitoredTransactions().map(_.data).collect{case t: TLBundleD => t}

      // Transactions
      for (out <- output) {
        println(out)
      }

      // State Map
      val hash = sDriver.state
      for (x <- hash.mem.keys) {
        print(s"(${x}, ${hash.mem(x)}), ")
      }
      println("")
    }
  }
}
