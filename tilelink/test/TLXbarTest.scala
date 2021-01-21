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
  implicit val p: Parameters = new WithoutTLMonitors

  it should "VerifTL Test TLXbarRAM with directed transactions basic" in {
    val TLRAMSlave = LazyModule(new XBarToRAMStandalone)
    test(TLRAMSlave.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      // Multi Driver/Monitor
      val driver = new TLDriverMaster(c.clock, TLRAMSlave.in)
      val monitor = new TLMonitor(c.clock, TLRAMSlave.in)
      val simCycles = 500

      implicit val params = TLRAMSlave.in.params
      val inputTransactions = Seq(
        Put(0x0, 0x3333),
        Get(0x8),
        Get(0x8),
        Get(0x8)
      )

      driver.push(inputTransactions)
      c.clock.step(simCycles)

      val output = monitor.getMonitoredTransactions().map(_.data).collect{case t:TLBundleD => t}

      for (out <- output) {
        println(out)
      }
    }
  }

  it should "VerifTL Test TLRAM XBar Multi-RAM with HW reference" in {
    val TLRAMSlave = LazyModule(new VerifTLMSXbarRAMSlaveReferenceStandalone)
    test(TLRAMSlave.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      // XBar DUT
      val dutDriver = new TLDriverMaster(c.clock, TLRAMSlave.in)
      val dutMonitor = new TLMonitor(c.clock, TLRAMSlave.in)

      // HW Reference
      val refDriver = new TLDriverMaster(c.clock, TLRAMSlave.inRef)
      val refMonitor = new TLMonitor(c.clock, TLRAMSlave.inRef)

      val simCycles = 500

      implicit val params = TLRAMSlave.in.params
      val inputTransactions = Seq(
        Put(0x0, 0x3333),
        Get(0x0),
        Get(0x0),
        Put(0x100, 0x5555),
        Get(0x100),
        Get(0x100)
      )

      dutDriver.push(inputTransactions)
      refDriver.push(inputTransactions)
      c.clock.step(simCycles)

      val output = dutMonitor.getMonitoredTransactions().map(_.data).collect{case t:TLBundleD => t}
      val outputRef = refMonitor.getMonitoredTransactions().map(_.data).collect{case t:TLBundleD => t}

      output.zip(outputRef).foreach {
        case (dut_out, sw_out) =>
          assert(dut_out.data == sw_out.data)
      }
    }
  }

  it should "VerifTL Test TLRAM XBar Multi-Master - Basic Directed Test" in {
    val TLRAMSlave = LazyModule(new VerifTLMMXbarRAMSlaveStandalone)
    test(TLRAMSlave.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      // Master 1
      val driver1 = new TLDriverMaster(c.clock, TLRAMSlave.inOne)
      val monitor1 = new TLMonitor(c.clock, TLRAMSlave.inOne)

      // Master 2
      val driver2 = new TLDriverMaster(c.clock, TLRAMSlave.inTwo)
      val monitor2 = new TLMonitor(c.clock, TLRAMSlave.inTwo)

      val simCycles = 500

      // Note: What to do in cases where there are multiple params?
      implicit val params = TLRAMSlave.inOne.params
      val tx1 = Seq(
        Put(0x0, 0x1111),
        Put(0x8, 0x2222),
        Put(0x10, 0x3333),
        Put(0x18, 0x4444),
        Get(0x20),
        Get(0x28),
        Get(0x30),
        Get(0x38)
      )

      val tx2 = Seq(
        Put(0x20, 0x5555),
        Put(0x28, 0x6666),
        Put(0x30, 0x7777),
        Put(0x38, 0x8888),
        Get(0x0),
        Get(0x8),
        Get(0x10),
        Get(0x18)
      )

      driver1.push(tx1)
      driver2.push(tx2)
      c.clock.step(simCycles)

      val out1 = monitor1.getMonitoredTransactions().map(_.data).collect{case t:TLBundleD => t}
      val sanity1 = new TLSanityChecker(TLRAMSlave.inOne.params, standaloneSlaveParams.managers.head, standaloneMasterParams.clients.head)
      sanity1.sanityCheck(out1)
      val out2 = monitor2.getMonitoredTransactions().map(_.data).collect{case t:TLBundleD => t}
      val sanity2 = new TLSanityChecker(TLRAMSlave.inTwo.params, standaloneSlaveParams.managers.head, standaloneMasterParams.clients.head)
      sanity2.sanityCheck(out2)

      // Hardcoded Reference Outputs
      // Note incorrect size, TODO FIX
      val out1Ref = Seq(
        AccessAck(0),
        AccessAck(0),
        AccessAck(0),
        AccessAck(0),
        AccessAckData(0x5555, 0),
        AccessAckData(0x6666, 0),
        AccessAckData(0x7777, 0),
        AccessAckData(0x8888, 0)
      )

      val out2Ref = Seq(
        AccessAck(0),
        AccessAck(0),
        AccessAck(0),
        AccessAck(0),
        AccessAckData(0x1111, 0),
        AccessAckData(0x2222, 0),
        AccessAckData(0x3333, 0),
        AccessAckData(0x4444, 0)
      )

      out1.zip(out1Ref).foreach {
        case (dut_out, sw_out) =>
          assert(dut_out.data == sw_out)
      }

      out2.zip(out2Ref).foreach {
        case (dut_out, sw_out) =>
          assert(dut_out.data == sw_out)
      }
    }
  }
}