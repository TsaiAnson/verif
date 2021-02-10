package verif

import chiseltest._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.tilelink.{TLBundleD, TLBundleParameters}
import org.scalatest.flatspec.AnyFlatSpec
import verif.TLTransaction.{AccessAck, AccessAckData, Get, Put}

class TLRegBankTest extends AnyFlatSpec with ChiselScalatestTester {
  it should "Test the standalone TL reg bank via directed stimulus" in {
    val TLRegBankSlave = LazyModule(new TLRegBankStandalone)

    test(TLRegBankSlave.module) { c =>
      val driver = new TLDriverMaster(c.clock, TLRegBankSlave.in)
      val slaveParams = TLRegBankSlave.regNode.edges.in.head.slave.slaves.head
      val masterParams = TLRegBankSlave.regNode.edges.in.head.master.masters.head
      val protocolChecker = new TLProtocolChecker(TLRegBankSlave.in.params, slaveParams, masterParams)
      val monitor = new TLMonitor(c.clock, TLRegBankSlave.in, Some(protocolChecker))

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

      val dispMonitor = new TLMonitor(c.clock, TLRegBankSlave.in)
      val dispatcher = new TLUDispatcher(TLRegBankSlave.in.params, None, inputTransactions)
      for (_ <- 0 until 40) {
        val txns = dispatcher.next(dispMonitor.getMonitoredTransactions().map({_.data}))
        driver.push(txns)
        c.clock.step(5)
      }

      val output = monitor.getMonitoredTransactions().map(_.data).collect { case t: TLBundleD => t }

      // TODO Add software model here
      val swoutput = Array(
        AccessAckData(0x0),
        AccessAckData(0x0),
        AccessAckData(0x0),
        AccessAckData(0x0),
        AccessAck(),
        AccessAck(),
        AccessAck(),
        AccessAck(),
        AccessAckData(0x0),
        AccessAckData(0x1),
        AccessAckData(0x2),
        AccessAckData(0x3)
      )

      output.zip(swoutput).foreach {
        case (dutOut, swOut) =>
          assert(dutOut.data.litValue() == swOut.data.litValue())
      }
    }
  }
}