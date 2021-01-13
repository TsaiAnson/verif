package verif

import chipsalliance.rocketchip.config.Parameters
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.WithoutTLMonitors
import freechips.rocketchip.tilelink.{TLBundleD, TLBundleParameters}
import org.scalatest.flatspec.AnyFlatSpec
import verif.TLTransaction.{AccessAck, AccessAckData, Get, Put}

class TLRegBankTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = new WithoutTLMonitors

  it should "Test the standalone TL reg bank via directed stimulus" in {
    val TLRegBankSlave = LazyModule(new TLRegBankStandalone)
    test(TLRegBankSlave.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val driver = new TLDriverMaster(c.clock, TLRegBankSlave.in)
      val monitor = new TLMonitor(c.clock, TLRegBankSlave.in)
      val simCycles = 100

      implicit val params: TLBundleParameters = TLRegBankSlave.in.params
      val inputTransactions = Seq(
        // Read back the values in registers 0x00, 0x08, 0x10, 0x18
        Get(0x0),
        Get(0x08),
        Get(0x10),
        Get(0x18),
        // Write values into registers 0x00, 0x08, 0x10, 0x18
        Put(0x0, 0),
        Put(0x8, 1),
        Put(0x10, 2),
        Put(0x18, 3),
        // Read back the values in registers 0x00, 0x08, 0x10, 0x18
        Get(0x0),
        Get(0x08),
        Get(0x10),
        Get(0x18)
      )

      driver.push(inputTransactions)
      c.clock.step(simCycles)

      val output = monitor.getMonitoredTransactions().map(_.data).collect { case t: TLBundleD => t }

      // TODO Add software model here
      val swoutput = Array(
        AccessAckData(data = 0x0, denied = 0),
        AccessAckData(data = 0x0, denied = 0),
        AccessAckData(data = 0x0, denied = 0),
        AccessAckData(data = 0x0, denied = 0),
        AccessAck(denied = 0),
        AccessAck(denied = 0),
        AccessAck(denied = 0),
        AccessAck(denied = 0),
        AccessAckData(data = 0x0, denied = 0),
        AccessAckData(data = 0x1, denied = 0),
        AccessAckData(data = 0x2, denied = 0),
        AccessAckData(data = 0x3, denied = 0)
      )

      output.zip(swoutput).foreach {
        case (dutOut, swOut) =>
          assert(dutOut.data.litValue() == swOut.data.litValue())
      }
    }
  }
}