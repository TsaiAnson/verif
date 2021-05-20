package verif

import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.WithoutTLMonitors
import TLTransaction._
import freechips.rocketchip.tilelink.TLBundleD
import verif.TLUtils._

class TLXbarTest extends AnyFlatSpec with ChiselScalatestTester {
  it should "VerifTL Test TLXbarRAM with directed transactions basic" in {
    val xbar = LazyModule(new XBarToRAMStandalone)
    test(xbar.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      // Multi Driver/Monitor
      val driver = new TLDriverMaster(c.clock, xbar.in)
      val protocolChecker = new TLProtocolChecker(xbar.mPortParams, xbar.sPortParams)
      val monitor = new TLMonitor(c.clock, xbar.in, Some(protocolChecker))

      implicit val params = xbar.in.params
      val inputTransactions = Seq(
        Put(0x0, 0x3333),
        Get(0x8),
        Get(0x8),
        Get(0x8)
      )

      val dispMonitor = new TLMonitor(c.clock, xbar.in)
      val dispatcher = new TLUFuzzer(xbar.in.params, None, inputTransactions)
      for (_ <- 0 until 30) {
        val txns = dispatcher.next(dispMonitor.getMonitoredTransactions().map({_.data}))
        driver.push(txns)
        c.clock.step(5)
      }

      val output = monitor.getMonitoredTransactions().map(_.data).collect{case t:TLBundleD => t}

      for (out <- output) {
        println(out)
      }
    }
  }

  it should "VerifTL Test TLRAM XBar Multi-RAM with HW reference" in {
    val TLRAMSlave = LazyModule(new XBarToMultiRAMStandalone)
    test(TLRAMSlave.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      // XBar DUT
      val dutDriver = new TLDriverMaster(c.clock, TLRAMSlave.in)
      val dutProtocolChecker = new TLProtocolChecker(TLRAMSlave.mPortParams, TLRAMSlave.sPortParams)
      val dutMonitor = new TLMonitor(c.clock, TLRAMSlave.in, Some(dutProtocolChecker))

      // HW Reference
      val refDriver = new TLDriverMaster(c.clock, TLRAMSlave.inRef)
      val refProtocolChecker = new TLProtocolChecker(TLRAMSlave.mPortParams, TLRAMSlave.sPortParams)
      val refMonitor = new TLMonitor(c.clock, TLRAMSlave.inRef, Some(refProtocolChecker))

      implicit val params = TLRAMSlave.in.params
      val inputTransactions = Seq(
        Put(0x0, 0x3333),
        Get(0x0),
        Get(0x0),
        Put(0x100, 0x5555),
        Get(0x100),
        Get(0x100)
      )

      val dutDispMonitor = new TLMonitor(c.clock, TLRAMSlave.in)
      val dutDispatcher = new TLUFuzzer(TLRAMSlave.in.params, None, inputTransactions)
      for (_ <- 0 until 30) {
        val txns = dutDispatcher.next(dutDispMonitor.getMonitoredTransactions().map({_.data}))
        dutDriver.push(txns)
        c.clock.step(5)
      }

      val refDispMonitor = new TLMonitor(c.clock, TLRAMSlave.in)
      val refDispatcher = new TLUFuzzer(TLRAMSlave.in.params, None, inputTransactions)
      for (_ <- 0 until 30) {
        val txns = refDispatcher.next(refDispMonitor.getMonitoredTransactions().map({_.data}))
        refDriver.push(txns)
        c.clock.step(5)
      }

      val output = dutMonitor.getMonitoredTransactions().map(_.data).collect{case t:TLBundleD => t}
      val outputRef = refMonitor.getMonitoredTransactions().map(_.data).collect{case t:TLBundleD => t}

      output.zip(outputRef).foreach {
        case (dut_out, sw_out) =>
          assert(dut_out.litValue() == sw_out.litValue())
      }
    }
  }

  it should "VerifTL Test TLRAM XBar Multi-Master - Basic Directed Test" in {
    val TLRAMSlave = LazyModule(new XbarToRAMMultiMasterStandalone)
    test(TLRAMSlave.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      // Master 1
      val driver1 = new TLDriverMaster(c.clock, TLRAMSlave.inOne)
      val protocolChecker1 = new TLProtocolChecker(TLRAMSlave.mPortParams.head, TLRAMSlave.sPortParams.head)
      val monitor1 = new TLMonitor(c.clock, TLRAMSlave.inOne, Some(protocolChecker1))

      // Master 2
      val driver2 = new TLDriverMaster(c.clock, TLRAMSlave.inTwo)
      val protocolChecker2 = new TLProtocolChecker(TLRAMSlave.mPortParams(1), TLRAMSlave.sPortParams(1))
      val monitor2 = new TLMonitor(c.clock, TLRAMSlave.inTwo, Some(protocolChecker2))

      // Note: What to do in cases where there are multiple params?
      implicit val params = TLRAMSlave.inOne.params
      val tx1 = Seq(
        Put(0x0, 0x1111, 0),
        Put(0x8, 0x2222, 0),
        Put(0x10, 0x3333, 0),
        Put(0x18, 0x4444, 0),
        Get(0x20, 0),
        Get(0x28, 0),
        Get(0x30, 0),
        Get(0x38, 0)
      )

      val tx2 = Seq(
        Put(0x20, 0x5555, 1),
        Put(0x28, 0x6666, 1),
        Put(0x30, 0x7777, 1),
        Put(0x38, 0x8888, 1),
        Get(0x0, 1),
        Get(0x8, 1),
        Get(0x10, 1),
        Get(0x18, 1)
      )

      val dispMonitorOne = new TLMonitor(c.clock, TLRAMSlave.inOne)
      val dispatcherOne = new TLUFuzzer(TLRAMSlave.inOne.params, None, tx1)
      val dispMonitorTwo = new TLMonitor(c.clock, TLRAMSlave.inTwo)
      val dispatcherTwo = new TLUFuzzer(TLRAMSlave.inTwo.params, None, tx2)
      for (_ <- 0 until 30) {
        val txns1 = dispatcherOne.next(dispMonitorOne.getMonitoredTransactions().map({_.data}))
        driver1.push(txns1)
        val txns2 = dispatcherTwo.next(dispMonitorTwo.getMonitoredTransactions().map({_.data}))
        driver2.push(txns2)
        c.clock.step(5)
      }
      val out1 = monitor1.getMonitoredTransactions().map(_.data).collect{case t:TLBundleD => t}
      val out2 = monitor2.getMonitoredTransactions().map(_.data).collect{case t:TLBundleD => t}

      // Hardcoded Reference Outputs
      // Note incorrect size, TODO FIX
      val out1Ref = Seq(
        AccessAck(),
        AccessAck(),
        AccessAck(),
        AccessAck(),
        AccessAckData(0x5555),
        AccessAckData(0x6666),
        AccessAckData(0x7777),
        AccessAckData(0x8888)
      )

      val out2Ref = Seq(
        AccessAck(0, 1),
        AccessAck(0, 1),
        AccessAck(0, 1),
        AccessAck(0, 1),
        AccessAckData(0x1111, 1),
        AccessAckData(0x2222, 1),
        AccessAckData(0x3333, 1),
        AccessAckData(0x4444, 1)
      )

      out1.zip(out1Ref).foreach {
        case (dut_out, sw_out) =>
          assert(dut_out.litValue() == sw_out.litValue())
      }

      out2.zip(out2Ref).foreach {
        case (dut_out, sw_out) =>
          assert(dut_out.litValue() == sw_out.litValue())
      }
    }
  }
}