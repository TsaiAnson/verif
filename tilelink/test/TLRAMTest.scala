package verif

import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule}
import freechips.rocketchip.subsystem.WithoutTLMonitors
import verif.TLUtils._
import TLTransaction._
import freechips.rocketchip.tilelink.{TLBundleA, TLBundleD, TLBundleParameters}

class TLRAMTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = new WithoutTLMonitors

  // TODO: unify these tests since they only differ in stimulus
  it should "Test standalone TL RAM using TLFuzzer" in {
    val TLRAM = LazyModule(new TLRAMStandalone)
    test(TLRAM.module).withAnnotations(Seq(TreadleBackendAnnotation, StructuralCoverageAnnotation, WriteVcdAnnotation)) { c =>
      val driver = new TLDriverMaster(c.clock, TLRAM.in)
      val monitor = new TLMonitor(c.clock, TLRAM.in)
      val simCycles = 400

      val fuz = new TLTransactionGenerator(standaloneSlaveParams.managers.head, TLRAM.in.params, overrideAddr = Some(AddressSet(0x00, 0x1ff)),
        burst = true, arith = true, logic = true)
      val inputTransactions = fuz.generateTransactions(60)

      driver.push(inputTransactions)
      c.clock.step(simCycles)

      val output = monitor.getMonitoredTransactions().map(_.data).collect{ case t: TLBundleD => t}
      val sanity = new TLSanityChecker(TLRAM.in.params, standaloneSlaveParams.managers.head, standaloneMasterParams.clients.head)
      sanity.sanityCheck(output)

      // No SW output checking as RAMModel checks for correctness
    }
  }

  it should "Test standalone TL RAM with a hardcoded burst" in {
    val TLRAM = LazyModule(new TLRAMStandalone)
    test(TLRAM.module).withAnnotations(Seq(TreadleBackendAnnotation, StructuralCoverageAnnotation, WriteVcdAnnotation)) { c =>
      implicit val params: TLBundleParameters = TLRAM.in.params

      val driver = new TLDriverMaster(c.clock, TLRAM.in)
      val monitor = new TLMonitor(c.clock, TLRAM.in)
      val simCycles = 150

      val inputTxns: Seq[TLBundleA] = Seq(
          Put(addr = 0x0, data = 0x3333),
          Get(addr = 0x0)
        ) ++
          PutBurst(addr = 0x10, data = Seq(0x1234, 0x5678), source = 0) :+
          Get(addr = 0x10)

      driver.push(inputTxns)
      c.clock.step(simCycles)

      // TODO: replace with software RAM model
      val expectedOut = Seq(
        AccessAck(0),
        AccessAckData(0x3333, 0),
        AccessAck(0, 4),
        AccessAckData(0x1234, 0)
      )

      val output = monitor.getMonitoredTransactions().map(_.data).collect{ case t: TLBundleD => t}
      val sanity = new TLSanityChecker(TLRAM.in.params, standaloneSlaveParams.managers.head, standaloneMasterParams.clients.head)
      sanity.sanityCheck(output)

      output.zip(expectedOut).foreach {
        case (dutOut, expOut) =>
          assert(dutOut.opcode.litValue() == expOut.opcode.litValue())
          assert(dutOut.denied.litValue() == expOut.denied.litValue())
          if (dutOut.opcode.litValue() == TLOpcodes.AccessAckData) {
            assert(dutOut.data.litValue() == expOut.data.litValue())
          }
      }
    }
  }

  it should "Basic Unittest of UH Transactions (Atomics, Hints)" in {
    val TLRAMSlave = LazyModule(new TLRAMStandalone)
    test(TLRAMSlave.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val driver = new TLDriverMaster(c.clock, TLRAMSlave.in)
      val monitor = new TLMonitor(c.clock, TLRAMSlave.in)
      val simCycles = 150

      implicit val params: TLBundleParameters = TLRAMSlave.in.params
      // Hints fail due to assertion in RAMModel
      val inputTransactions = {
        Seq(
          Put(addr = 0x0, data = 0x1234),
          Get(addr = 0x0),
          Arith(param = 4, addr = 0x0, data = 0x1),
          Get(addr = 0x0),
          Logic(param = 2, addr = 0x0, data = 0xfff0),
          Get(addr = 0x0)
        )
      }

      driver.push(inputTransactions)
      c.clock.step(simCycles)

      val output = monitor.getMonitoredTransactions().map(_.data).collect{case t: TLBundleD => t}
      val sanity = new TLSanityChecker(TLRAMSlave.in.params, standaloneSlaveParams.managers.head, standaloneMasterParams.clients.head)
      sanity.sanityCheck(output)

      for (out <- output) {
        println(out)
      }
    }
  }

  // TODO: inject a cover for 4 consecutive writes in TLRAM and check that this test hits it
  it should "TLRAM Throughput Test" in {
    val TLRAMSlave = LazyModule(new TLRAMStandalone)
    test(TLRAMSlave.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val driver = new TLDriverMaster(c.clock, TLRAMSlave.in)
      val monitor = new TLMonitor(c.clock, TLRAMSlave.in)
      val simCycles = 150

      implicit val params: TLBundleParameters = TLRAMSlave.in.params
      // Four Consecutive Writes (burst)
      val inputTransactions = PutBurst(addr = 0x10, data = Seq(0x1234, 0x5678, 0x8765, 0x4321), source = 0)

      driver.push(inputTransactions)
      c.clock.step(simCycles)

      val output = monitor.getMonitoredTransactions().map(_.data).collect{case t: TLBundleD => t}
      val sanity = new TLSanityChecker(TLRAMSlave.in.params, standaloneSlaveParams.managers.head, standaloneMasterParams.clients.head)
      sanity.sanityCheck(output)

      for (out <- output) {
        println(out)
      }
    }
  }

  // This test just provides a reference output for the corresponding test in SlaveDriverTest
  it should "test TLRAM with longer stimulus" in {
    val TLRAMSlave = LazyModule(new TLRAMStandalone)
    test(TLRAMSlave.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val driver = new TLDriverMaster(c.clock, TLRAMSlave.in)
      val monitor = new TLMonitor(c.clock, TLRAMSlave.in)
      val simCycles = 150

      implicit val params: TLBundleParameters = TLRAMSlave.in.params
      // Four Consecutive Writes (burst)
      val inputTransactions: Seq[TLBundleA] = Seq(
        Get(0x0),
        Put(0x0, 0x3333),
        Get(0x0)
      ) ++
        (PutBurst(0x0, Seq(0x5555, 0x1234), 0) :+
          Get(0x0) :+
          Get(0x8)) ++
        (LogicBurst(2, 0x0, Seq(0x0, 0x0)) :+
          Get(0x0) :+
          Get(0x8)) ++
        ArithBurst(4, 0x0, Seq(0x2222, 0x8888)) :+
        Get(0x0) :+
        Get(0x8)

      driver.push(inputTransactions)
      c.clock.step(simCycles)

      val output = monitor.getMonitoredTransactions().map(_.data).collect{case t: TLBundleD => t}
      val sanity = new TLSanityChecker(TLRAMSlave.in.params, standaloneSlaveParams.managers.head, standaloneMasterParams.clients.head)
      sanity.sanityCheck(output)

      for (out <- output) {
        println(out.opcode, out.data, out.size)
      }
    }
  }
}
