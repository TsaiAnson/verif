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
      val monitor = new TLMonitor(c.clock, pusher.out)

      c.clock.step(100)

      val monitored = monitor.getMonitoredTransactions().map(_.data).collect{ case t: TLBundleD => t}
      val expected = Seq(AccessAck(0), AccessAckData(100, 0))
      // TODO: check equality
    }
  }

  it should "handle transactions from the synthesizable TLFuzzer without errors" in {
    val N = 30
    val fuzzer = LazyModule(new TLFuzzerStandalone(N))
    test(fuzzer.module).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      implicit val bundleParams: TLBundleParameters = fuzzer.out.params
      val slaveFn = new TLMemoryModel(bundleParams)
      val slaveModel = new TLDriverSlave(c.clock, fuzzer.out, slaveFn, TLMemoryModel.State.empty())
      val monitor = new TLMonitor(c.clock, fuzzer.out)

      c.clock.step(100)

      val output = monitor.getMonitoredTransactions()
      assert(output.length == 60)
    }
  }

  it should "handle transactions driven by the TLDriverMaster" in {
    val passthrough = LazyModule(new TLBufferStandalone)
    test(passthrough.module).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      implicit val bundleParams: TLBundleParameters = passthrough.in.params
      val mDriver = new TLDriverMaster(c.clock, passthrough.in)
      val slaveFn = new TLMemoryModel(bundleParams)
      val sDriver = new TLDriverSlave(c.clock, passthrough.out, slaveFn, TLMemoryModel.State.empty())
      val monitor = new TLMonitor(c.clock, passthrough.in)

      // TODO: fetch stimulus from memory sequences
      val stimulus = Seq.empty[TLChannel]
      val expected = Seq.empty[TLChannel]
      mDriver.push(stimulus)
      c.clock.step(100)

      val output = monitor.getMonitoredTransactions().map(_.data).collect{case t: TLBundleD => t}
      // TODO: add check
    }
  }
}
