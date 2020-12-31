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
import verifTLUtils._

class TLRAMTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = new WithoutTLMonitors

  it should "VerifTL Test TLRAM via SWTLFuzzer" in {
    val TLRAMSlave = LazyModule(new VerifTLRAMSlave)
    test(TLRAMSlave.module).withAnnotations(Seq(VerilatorBackendAnnotation, StructuralCoverageAnnotation, WriteVcdAnnotation)) { c =>

      val passInAgent = new TLDriverMaster(c.clock, TLRAMSlave.in)
      val passOutAgent = new TLMonitor(c.clock, TLRAMSlave.in)
      val simCycles = 200

      val fuz = new SWTLFuzzer(standaloneSlaveParams.managers(0), overrideAddr = Some(AddressSet(0x00, 0x1ff)))
      val inputTransactions = fuz.generateTransactions(60)

      passInAgent.push(inputTransactions)
      c.clock.step(simCycles)

      val output = passOutAgent.getMonitoredTransactions(filterD).toArray

      // No SW output checking as RAMModel checks for correctness
    }
  }

  it should "Driver/Monitor Master Hardcoded Burst TLRAM" in {
    val TLRAMSlave = LazyModule(new VerifTLRAMSlave)
    test(TLRAMSlave.module).withAnnotations(Seq(VerilatorBackendAnnotation, StructuralCoverageAnnotation, WriteVcdAnnotation)) { c =>

      val passInAgent = new TLDriverMaster(c.clock, TLRAMSlave.in)
      val passOutAgent = new TLMonitor(c.clock, TLRAMSlave.in)
      val simCycles = 150

      val inputTransactions = Seq(
        PutFull(source = 0.U, addr = 0x0.U, mask = 0xff.U, data = 0x3333.U),
        Get(size = 3.U, source = 0.U, addr = 0x0.U, mask = 0xff.U),
        PutFullBurst(size = 4.U, source = 0.U, addr = 0x10.U, masks = List(0xff.U, 0xff.U), datas = List(0x1234.U, 0x5678.U)),
        Get(size = 4.U, source = 0.U, addr = 0x10.U, mask = 0xff.U)
      )

      passInAgent.push(inputTransactions)
      c.clock.step(simCycles)

      val output = passOutAgent.getMonitoredTransactions(filterD).toArray

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

      val inputTransactions = Seq(
        // Hints fail due to assertion in RAMModel
//        Intent(param = 1.U, size = 3.U, source = 0.U, addr = 0x0.U, mask = 0xff.U), PutFull(source = 0.U, addr = 0x0.U, mask = 0xff.U, data = 0x1234.U),
//        Intent(param = 1.U, size = 3.U, source = 0.U, addr = 0x0.U, mask = 0xff.U), Get(size = 3.U, source = 0.U, addr = 0x0.U, mask = 0xff.U),
        PutFull(source = 0.U, addr = 0x0.U, mask = 0xff.U, data = 0x1234.U),
        Get(size = 3.U, source = 0.U, addr = 0x0.U, mask = 0xff.U),
        ArithData(param = 4.U, source = 0.U, addr = 0x0.U, mask = 0xff.U, data = 0x1.U), Get(size = 3.U, source = 0.U, addr = 0x0.U, mask = 0xff.U),
        LogicData(param = 2.U, source = 0.U, addr = 0x0.U, mask = 0xff.U, data = 0xfff0.U), Get(size = 3.U, source = 0.U, addr = 0x0.U, mask = 0xff.U)
      )

      passInAgent.push(inputTransactions)
      c.clock.step(simCycles)

      val output = passOutAgent.getMonitoredTransactions(filterD).toArray

      for (out <- output) {
        println(out)
      }
    }
  }

  it should "TLRAM Throughput Test" in {
    val TLRAMSlave = LazyModule(new VerifTLRAMSlave)
    test(TLRAMSlave.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>

      val passInAgent = new TLDriverMaster(c.clock, TLRAMSlave.in)
      val passOutAgent = new TLMonitor(c.clock, TLRAMSlave.in)
      val simCycles = 150

      val inputTransactions = Seq(
//        // Four Consecutive Writes (singles)
//        PutFull(source = 0.U, addr = 0x0.U, mask = 0xff.U, data = 0x1234.U),
//        PutFull(source = 0.U, addr = 0x0.U, mask = 0xff.U, data = 0x1234.U),
//        PutFull(source = 0.U, addr = 0x0.U, mask = 0xff.U, data = 0x1234.U),
//        PutFull(source = 0.U, addr = 0x0.U, mask = 0xff.U, data = 0x1234.U)
        // Four Consecutive Writes (burst)
        PutFullBurst(size = 5.U, source = 0.U, addr = 0x10.U, masks = List(0xff.U, 0xff.U, 0xff.U, 0xff.U),
          datas = List(0x1234.U, 0x5678.U, 0x8765.U, 0x4321.U))
      )

      passInAgent.push(inputTransactions)
      c.clock.step(simCycles)

      val output = passOutAgent.getMonitoredTransactions(filterD).toArray

      for (out <- output) {
        println(out)
      }
    }
  }
}
