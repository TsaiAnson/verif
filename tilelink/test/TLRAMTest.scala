package verif

import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest._
import designs._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule}
import freechips.rocketchip.subsystem.WithoutTLMonitors
import verif.TLUtils._
import TLTransaction._
import freechips.rocketchip.tilelink.TLChannel

class TLRAMTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = new WithoutTLMonitors

  it should "VerifTL Test TLRAM via SWTLFuzzer" in {
    val TLRAMSlave = LazyModule(new VerifTLRAMSlave)
    test(TLRAMSlave.module).withAnnotations(Seq(TreadleBackendAnnotation, StructuralCoverageAnnotation, WriteVcdAnnotation)) { c =>
      val passInAgent = new TLDriverMaster(c.clock, TLRAMSlave.in)
      val passOutAgent = new TLMonitor(c.clock, TLRAMSlave.in)
      val simCycles = 400

      val fuz = new SWTLFuzzer(standaloneSlaveParams.managers(0), TLRAMSlave.in.params, overrideAddr = Some(AddressSet(0x00, 0x1ff)),
        burst = true, arith = true, logic = true)
      val inputTransactions = fuz.generateTransactions(60)

      passInAgent.push(inputTransactions)
      c.clock.step(simCycles)

      val output = passOutAgent.getMonitoredTransactions().filter(filterD).toArray
      println("TRANSACTIONS TOTAL")
      println(output.size)

      // No SW output checking as RAMModel checks for correctness
    }
  }

  it should "Driver/Monitor Master Hardcoded Burst TLRAM" in {
    val TLRAMSlave = LazyModule(new VerifTLRAMSlave)
    test(TLRAMSlave.module).withAnnotations(Seq(TreadleBackendAnnotation, StructuralCoverageAnnotation, WriteVcdAnnotation)) { c =>
      implicit val params = TLRAMSlave.in.params

      val passInAgent = new TLDriverMaster(c.clock, TLRAMSlave.in)
      val passOutAgent = new TLMonitor(c.clock, TLRAMSlave.in)
      val simCycles = 150

      val inputTxns = Seq(
        Seq(Put(addr = 0x0, data = 0x3333)),
        Seq(Get(addr = 0x0)),
        PutBurst(addr = 0x10, data = Seq(0x1234, 0x5678), source = 0),
        Seq(Get(addr = 0x10))
      ).flatten

      passInAgent.push(inputTxns)
      c.clock.step(simCycles)

      val output = passOutAgent.getMonitoredTransactions().filter(filterD).toArray

      for (out <- output) {
        println(out)
      }
    }
  }

  it should "Basic Unittest of UH Transactions (Atomics, Hints)" in {
    val TLRAMSlave = LazyModule(new VerifTLRAMSlave)
    test(TLRAMSlave.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      val passInAgent = new TLDriverMaster(c.clock, TLRAMSlave.in)
      val passOutAgent = new TLMonitor(c.clock, TLRAMSlave.in)
      val simCycles = 150

      implicit val params = TLRAMSlave.in.params
      val inputTransactions = Seq(
        // Hints fail due to assertion in RAMModel
        Put(addr = 0x0, data = 0x1234),
        Get(addr = 0x0),
        Arith(param = 4, addr = 0x0, data = 0x1),
        Get(addr = 0x0),
        Logic(param = 2, addr = 0x0, data = 0xfff0),
        Get(addr = 0x0)
      )

      passInAgent.push(inputTransactions)
      c.clock.step(simCycles)

      val output = passOutAgent.getMonitoredTransactions().filter(filterD).toArray

      for (out <- output) {
        println(out)
      }
    }
  }

  it should "TLRAM Throughput Test" in {
    val TLRAMSlave = LazyModule(new VerifTLRAMSlave)
    test(TLRAMSlave.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      val passInAgent = new TLDriverMaster(c.clock, TLRAMSlave.in)
      val passOutAgent = new TLMonitor(c.clock, TLRAMSlave.in)
      val simCycles = 150

      implicit val params = TLRAMSlave.in.params
      val inputTransactions =
        // Four Consecutive Writes (burst)
        PutBurst(addr = 0x10, data = Seq(0x1234, 0x5678, 0x8765, 0x4321), source = 0)

      passInAgent.push(inputTransactions)
      c.clock.step(simCycles)

      val output = passOutAgent.getMonitoredTransactions().filter(filterD).toArray

      for (out <- output) {
        println(out)
      }
    }
  }
}
