package verif

import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.WithoutTLMonitors
import freechips.rocketchip.tilelink.{ReadPattern, TLBundleD, TLBundleParameters, TLChannel, WritePattern}

import TLTransaction._

class TLDriverSlaveTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = new WithoutTLMonitors

  behavior of "TLDriverSlave"
  it should "receive transactions from TLPatternPusher and respond correctly" in {
    val patterns = Seq(WritePattern(0x10, 3, 100), ReadPattern(0x10, 3))
    val pusher = LazyModule(new TLPatternPusherStandalone(patterns))

    test(pusher.module).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      implicit val bundleParams: TLBundleParameters = pusher.out.params
      val slaveFn = new TLMemoryModel(bundleParams)
      val slaveModel = new TLDriverSlave(c.clock, pusher.out, slaveFn, TLMemoryModel.State.empty())
      val protocolChecker = new TLProtocolChecker(pusher.out.params, pusher.sPortParams.head.managers.head, pusher.mPortParams.head.clients.head)
      val monitor = new TLMonitor(c.clock, pusher.out, Some(protocolChecker))

      c.clock.step(100)

      val monitored = monitor.getMonitoredTransactions().map(_.data).collect{ case t: TLBundleD => t}
      val expected = Seq(AccessAck(0), AccessAckData(100, 0))
      val comparison = equalsTL(monitored, expected)
      assert(comparison.isEmpty)
    }
  }

  it should "handle transactions from the synthesizable TLFuzzer without errors" in {
    val N = 30
    val fuzzer = LazyModule(new TLFuzzerStandalone(N))
    test(fuzzer.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      implicit val bundleParams: TLBundleParameters = fuzzer.out.params
      val slaveFn = new TLMemoryModel(bundleParams)
      val slaveModel = new TLDriverSlave(c.clock, fuzzer.out, slaveFn, TLMemoryModel.State.empty())
      val protocolChecker = new TLProtocolChecker(fuzzer.out.params, fuzzer.sPortParams.head.managers.head, fuzzer.mPortParams.head.clients.head)
      val monitor = new TLMonitor(c.clock, fuzzer.out, Some(protocolChecker))

      c.clock.step(100)

      val output = monitor.getMonitoredTransactions().map(_.data).collect{ case t: TLBundleD => t}
      assert(output.length == N)
    }
  }

  it should "handle transactions driven by the TLDriverMaster" in {
    val passthrough = LazyModule(new TLBufferStandalone)
    test(passthrough.module).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      implicit val bundleParams: TLBundleParameters = passthrough.in.params
      val mDriver = new TLDriverMaster(c.clock, passthrough.in)
      val slaveFn = new TLMemoryModel(bundleParams)
      val sDriver = new TLDriverSlave(c.clock, passthrough.out, slaveFn, TLMemoryModel.State.empty())
      val protocolChecker = new TLProtocolChecker(passthrough.in.params, passthrough.sPortParams.head.managers.head, passthrough.mPortParams.head.clients.head)
      val monitor = new TLMonitor(c.clock, passthrough.in, Some(protocolChecker))

      val (stimulus, expected) = (Seq(
        Put(0x8, BigInt("0123456789abcdef", 16)),
        Get(0x8),
        Put(0x8, BigInt("ffffffffffffffff", 16)),
        Get(0x8),
      ),
      Seq(
        AccessAck(0),
        AccessAckData(BigInt("0123456789abcdef", 16), 0),
        AccessAck(0),
        AccessAckData(BigInt("ffffffffffffffff", 16), 0)
      ))

      val dispMonitor = new TLMonitor(c.clock, passthrough.in)
      val dispatcher = new TLUDispatcher(passthrough.in.params, None, stimulus)
      for (_ <- 0 until 20) {
        val txns = dispatcher.next(dispMonitor.getMonitoredTransactions().map({_.data}))
        mDriver.push(txns)
        c.clock.step(5)
      }

      val output = monitor.getMonitoredTransactions().map(_.data).collect{case t: TLBundleD => t}
      val comparison = equalsTL(output, expected)
      assert(comparison.isEmpty)
    }
  }
}
