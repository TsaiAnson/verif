package verif

import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest._
import designs._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.WithoutTLMonitors
import verifTLUtils._
import TLTransaction._

class TLXbarTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = new WithoutTLMonitors

  it should "VerifTL Test TLXbarRAM with directed transactions basic" in {
    val TLRAMSlave = LazyModule(new VerifTLXbarRAMSimpleSlave)
    test(TLRAMSlave.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      // Multi Driver/Monitor
      val passInAgent = new TLDriverMaster(c.clock, TLRAMSlave.in)
      val passOutAgent = new TLMonitor(c.clock, TLRAMSlave.in)
      val simCycles = 500

      implicit val params = TLRAMSlave.in.params
      val inputTransactions = Seq(
        Put(addr = 0x0, data = 0x3333),
        Get(addr = 0x8),
        Get(addr = 0x8),
        Get(addr = 0x8)
      )

      passInAgent.push(inputTransactions)
      c.clock.step(simCycles)

      val output = passOutAgent.getMonitoredTransactions(filterD).toArray

      for (out <- output) {
        println(out)
      }
    }
  }

  it should "VerifTL Test TLRAM XBar Multi-RAM with HW reference" in {
    val TLRAMSlave = LazyModule(new VerifTLMSXbarRAMSlaveReferenceStandalone)
    test(TLRAMSlave.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      // XBar DUT
      val passInAgent = new TLDriverMaster(c.clock, TLRAMSlave.in)
      val passOutAgent = new TLMonitor(c.clock, TLRAMSlave.in)

      // HW Reference
      val passInAgentRef = new TLDriverMaster(c.clock, TLRAMSlave.inRef)
      val passOutAgentRef = new TLMonitor(c.clock, TLRAMSlave.inRef)

      val simCycles = 500

      implicit val params = TLRAMSlave.in.params
      val inputTransactions = Seq(
        Put(addr = 0x0, data = 0x3333),
        Get(addr = 0x0),
        Get(addr = 0x0),
        Put(addr = 0x100, data = 0x5555),
        Get(addr = 0x100),
        Get(addr = 0x100)
      )

      passInAgent.push(inputTransactions)
      passInAgentRef.push(inputTransactions)
      c.clock.step(simCycles)

      val output = passOutAgent.getMonitoredTransactions(filterD).toArray
      val outputRef = passOutAgent.getMonitoredTransactions(filterD).toArray

      assert(outputChecker.checkOutput(output, {t : TLTransaction => t},
        outputRef, {t : TLTransaction => t}))
    }
  }

  it should "VerifTL Test TLRAM XBar Multi-Master - Basic Directed Test" in {
    val TLRAMSlave = LazyModule(new VerifTLMMXbarRAMSlaveStandalone)
    test(TLRAMSlave.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      // Master 1
      val passInAgentOne = new TLDriverMaster(c.clock, TLRAMSlave.inOne)
      val passOutAgentOne = new TLMonitor(c.clock, TLRAMSlave.inOne)

      // Master 2
      val passInAgentTwo = new TLDriverMaster(c.clock, TLRAMSlave.inTwo)
      val passOutAgentTwo = new TLMonitor(c.clock, TLRAMSlave.inTwo)

      val simCycles = 500

      // Note: What to do in cases where there are multiple params?
      implicit val params = TLRAMSlave.inOne.params
      val inputTransactionsOne = Seq(
        Put(addr = 0x0, data = 0x1111),
        Put(addr = 0x8, data = 0x2222),
        Put(addr = 0x10, data = 0x3333),
        Put(addr = 0x18, data = 0x4444),
        Get(addr = 0x20),
        Get(addr = 0x28),
        Get(addr = 0x30),
        Get(addr = 0x38)
      )

      val inputTransactionsTwo = Seq(
        Put(addr = 0x20, data = 0x5555),
        Put(addr = 0x28, data = 0x6666),
        Put(addr = 0x30, data = 0x7777),
        Put(addr = 0x38, data = 0x8888),
        Get(addr = 0x0),
        Get(addr = 0x8),
        Get(addr = 0x10),
        Get(addr = 0x18)
      )

      passInAgentOne.push(inputTransactionsOne)
      passInAgentTwo.push(inputTransactionsTwo)
      c.clock.step(simCycles)

      val outputOne = passOutAgentOne.getMonitoredTransactions(filterD).toArray
      val outputTwo = passOutAgentTwo.getMonitoredTransactions(filterD).toArray

      // Hardcoded Reference Outputs
      // Note incorrect size, TODO FIX
      val outputOneRef = Seq(
        AccessAck(denied = 0),
        AccessAck(denied = 0),
        AccessAck(denied = 0),
        AccessAck(denied = 0),
        AccessAckData(data = 0x5555, denied = 0),
        AccessAckData(data = 0x6666, denied = 0),
        AccessAckData(data = 0x7777, denied = 0),
        AccessAckData(data = 0x8888, denied = 0)
      ).toArray

      val outputTwoRef = Seq(
        AccessAck(denied = 0),
        AccessAck(denied = 0),
        AccessAck(denied = 0),
        AccessAck(denied = 0),
        AccessAckData(data = 0x1111, denied = 0),
        AccessAckData(data = 0x2222, denied = 0),
        AccessAckData(data = 0x3333, denied = 0),
        AccessAckData(data = 0x4444, denied = 0)
      ).toArray

      assert(outputChecker.checkOutput(outputOne, {t : TLTransaction => t},
        outputOneRef, {t : TLTransaction => t}))
      assert(outputChecker.checkOutput(outputTwo, {t : TLTransaction => t},
        outputTwoRef, {t : TLTransaction => t}))
    }
  }
}