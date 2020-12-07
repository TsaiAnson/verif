package verif

import org.scalatest._
import chisel3._
import chiseltest._
import designs._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule}
import freechips.rocketchip.subsystem.WithoutTLMonitors

class TLXbarTest extends FlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = new WithoutTLMonitors

  it should "VerifTL Test TLXbarRAM with directed transactions basic" in {
    val TLRAMSlave = LazyModule(new VerifTLXbarRAMSimpleSlave with VerifTLStandaloneBlock)
    test(TLRAMSlave.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>

      // Multi Driver/Monitor
      val passInAgent = new TLDriverMaster(c.clock, TLRAMSlave.in)
      val passOutAgent = new TLMonitorMaster(c.clock, TLRAMSlave.in)
      val simCycles = 500

      val inputTransactions = Seq(
        PutFull(addr = 0x0.U, mask = 0xff.U, data = 0x3333.U),
        Get(size = 3.U, addr = 0x8.U, mask = 0xff.U),
        Get(size = 3.U, addr = 0x8.U, mask = 0xff.U),
        Get(size = 3.U, addr = 0x8.U, mask = 0xff.U)
      )

      passInAgent.push(inputTransactions)
      c.clock.step(simCycles)

      val output = passOutAgent.getMonitoredTransactions.toArray

      for (out <- output) {
        println(out)
      }
    }
  }

  it should "VerifTL Test TLRAM XBar Multi-RAM with HW reference" in {
    val TLRAMSlave = LazyModule(new VerifTLMSXbarRAMSlaveReferenceStandalone)
    test(TLRAMSlave.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>

      // XBar DUT
      val passInAgent = new TLDriverMaster(c.clock, TLRAMSlave.in)
      val passOutAgent = new TLMonitorMaster(c.clock, TLRAMSlave.in)

      // HW Reference
      val passInAgentRef = new TLDriverMaster(c.clock, TLRAMSlave.inRef)
      val passOutAgentRef = new TLMonitorMaster(c.clock, TLRAMSlave.inRef)

      val simCycles = 500

      val inputTransactions = Seq(
        PutFull(addr = 0x0.U, mask = 0xff.U, data = 0x3333.U),
        Get(size = 3.U, addr = 0x0.U, mask = 0xff.U),
        Get(size = 3.U, addr = 0x0.U, mask = 0xff.U),
        PutFull(addr = 0x100.U, mask = 0xff.U, data = 0x5555.U),
        Get(size = 3.U, addr = 0x100.U, mask = 0xff.U),
        Get(size = 3.U, addr = 0x100.U, mask = 0xff.U)
      )

      passInAgent.push(inputTransactions)
      passInAgentRef.push(inputTransactions)
      c.clock.step(simCycles)

      val output = passOutAgent.getMonitoredTransactions.toArray
      val outputRef = passOutAgentRef.getMonitoredTransactions.toArray

      assert(outputChecker.checkOutput(output, {t : TLTransaction => t},
        outputRef, {t : TLTransaction => t}))
    }
  }

  it should "VerifTL Test TLRAM XBar Multi-Master - Basic Directed Test" in {
    val TLRAMSlave = LazyModule(new VerifTLMMXbarRAMSlaveStandalone)
    test(TLRAMSlave.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>

      // Master 1
      val passInAgentOne = new TLDriverMaster(c.clock, TLRAMSlave.inOne)
      val passOutAgentOne = new TLMonitorMaster(c.clock, TLRAMSlave.inOne)

      // Master 2
      val passInAgentTwo = new TLDriverMaster(c.clock, TLRAMSlave.inTwo)
      val passOutAgentTwo = new TLMonitorMaster(c.clock, TLRAMSlave.inTwo)

      val simCycles = 500

      val inputTransactionsOne = Seq(
        PutFull(source = 0.U, addr = 0x0.U, mask = 0xff.U, data = 0x1111.U),
        PutFull(source = 0.U, addr = 0x8.U, mask = 0xff.U, data = 0x2222.U),
        PutFull(source = 0.U, addr = 0x10.U, mask = 0xff.U, data = 0x3333.U),
        PutFull(source = 0.U, addr = 0x18.U, mask = 0xff.U, data = 0x4444.U),
        Get(source = 0.U, size = 3.U, addr = 0x20.U, mask = 0xff.U),
        Get(source = 0.U, size = 3.U, addr = 0x28.U, mask = 0xff.U),
        Get(source = 0.U, size = 3.U, addr = 0x30.U, mask = 0xff.U),
        Get(source = 0.U, size = 3.U, addr = 0x38.U, mask = 0xff.U),
      )

      val inputTransactionsTwo = Seq(
        PutFull(source = 1.U, addr = 0x20.U, mask = 0xff.U, data = 0x5555.U),
        PutFull(source = 1.U, addr = 0x28.U, mask = 0xff.U, data = 0x6666.U),
        PutFull(source = 1.U, addr = 0x30.U, mask = 0xff.U, data = 0x7777.U),
        PutFull(source = 1.U, addr = 0x38.U, mask = 0xff.U, data = 0x8888.U),
        Get(source = 1.U, size = 3.U, addr = 0x0.U, mask = 0xff.U),
        Get(source = 1.U, size = 3.U, addr = 0x08.U, mask = 0xff.U),
        Get(source = 1.U, size = 3.U, addr = 0x10.U, mask = 0xff.U),
        Get(source = 1.U, size = 3.U, addr = 0x18.U, mask = 0xff.U),
      )

      passInAgentOne.push(inputTransactionsOne)
      passInAgentTwo.push(inputTransactionsTwo)
      c.clock.step(simCycles)

      val outputOne = passOutAgentOne.getMonitoredTransactions.toArray
      val outputTwo = passOutAgentTwo.getMonitoredTransactions.toArray

      // Hardcoded Reference Outputs
      // Note incorrect size, TODO FIX
      val outputOneRef = Seq(
        AccessAck(size = 3.U, denied =  false.B, source = 0.U),
        AccessAck(size = 3.U, denied =  false.B, source = 0.U),
        AccessAck(size = 3.U, denied =  false.B, source = 0.U),
        AccessAck(size = 3.U, denied =  false.B, source = 0.U),
        AccessAckData(size = 3.U, source = 0.U, denied = false.B, data = 0x5555.U),
        AccessAckData(size = 3.U, source = 0.U, denied = false.B, data = 0x6666.U),
        AccessAckData(size = 3.U, source = 0.U, denied = false.B, data = 0x7777.U),
        AccessAckData(size = 3.U, source = 0.U, denied = false.B, data = 0x8888.U)
      ).toArray

      val outputTwoRef = Seq(
        AccessAck(size = 3.U, denied =  false.B, source = 1.U),
        AccessAck(size = 3.U, denied =  false.B, source = 1.U),
        AccessAck(size = 3.U, denied =  false.B, source = 1.U),
        AccessAck(size = 3.U, denied =  false.B, source = 1.U),
        AccessAckData(size = 3.U, source = 1.U, denied = false.B, data = 0x1111.U),
        AccessAckData(size = 3.U, source = 1.U, denied = false.B, data = 0x2222.U),
        AccessAckData(size = 3.U, source = 1.U, denied = false.B, data = 0x3333.U),
        AccessAckData(size = 3.U, source = 1.U, denied = false.B, data = 0x4444.U)
      ).toArray

      assert(outputChecker.checkOutput(outputOne, {t : TLTransaction => t},
        outputOneRef, {t : TLTransaction => t}))
      assert(outputChecker.checkOutput(outputTwo, {t : TLTransaction => t},
        outputTwoRef, {t : TLTransaction => t}))
    }
  }
}