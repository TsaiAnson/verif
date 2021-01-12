package verif

import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest._
import designs._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.WithoutTLMonitors

import scala.collection.mutable.HashMap
import TLUtils._
import TLTransaction._
import freechips.rocketchip.tilelink.{TLBundleD, TLBundleE, TLBundleParameters, TLDataChannel}

class DSPToolsTest extends AnyFlatSpec with ChiselScalatestTester {
  // implicit val p: Parameters = Parameters.empty.asInstanceOf[Parameters]
  // implicit val p: Parameters = (new BaseConfig).toInstance
  implicit val p: Parameters = new WithoutTLMonitors

  it should "VerifTL Test Slave" in {
    val TLRegBankSlave = LazyModule(new VerifTLRegBankSlave)
    test(TLRegBankSlave.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val driver = new TLDriverMaster(c.clock, TLRegBankSlave.in)
      val monitor = new TLMonitor(c.clock, TLRegBankSlave.in)
      val simCycles = 100

      implicit val params: TLBundleParameters = TLRegBankSlave.in.params
      val inputTransactions = Seq(
        // Read back the values in registers 0x00, 0x08, 0x10, 0x18
        Get(addr = 0x0),
        Get(addr = 0x08),
        Get(addr = 0x10),
        Get(addr = 0x18),
        // Write values into registers 0x00, 0x08, 0x10, 0x18
        Put(addr = 0x0, data = 0),
        Put(addr = 0x8, data = 1),
        Put(addr = 0x10, data = 2),
        Put(addr = 0x18, data = 3),
        // Read back the values in registers 0x00, 0x08, 0x10, 0x18
        Get(addr = 0x0),
        Get(addr = 0x08),
        Get(addr = 0x10),
        Get(addr = 0x18)
      )

      driver.push(inputTransactions)
      c.clock.step(simCycles)

      val output = monitor.getMonitoredTransactions().filter {
        t =>
          t.data match {
            case _: TLBundleD => true
            case _ => false
          }
      }

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

      assert(output.length == swoutput.length)
      output.zip(swoutput).foreach {
        case (dutOut, swOut) =>
          assert(dutOut.data == swOut)
      }
    }
  }

  it should "VerifTL Test Master Fuzzer" in {
    val TLMasterFuzzer = LazyModule(new VerifTLMasterFuzzer)
    test(TLMasterFuzzer.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      implicit val params: TLBundleParameters = TLMasterFuzzer.out.params
      val requestHandler = new TLDriverSlave(c.clock, TLMasterFuzzer.out, SlaveMemoryState.init(), testResponseWrapper)
      val monitor = new TLMonitor(c.clock, TLMasterFuzzer.out)
      val simCycles = 100

      c.clock.step(simCycles)

      val output = monitor.getMonitoredTransactions().toArray

      // Sanity test to make sure that driver/monitor is correctly getting requests
      assert(output.length == 60)

//      for (out <- output) {
//        println(out.getElements)
//      }
    }
  }

//  it should "VerifTL Test Master Pattern" in {
//    // Currently hardcoded write values as Patterns does not support dependent patterns
//    // (e.g. write a value that was read in earlier pattern)
//    val mastertxns = Seq(ReadExpectPattern(0, 3, 10), WritePattern(0x20, 3, 10),
//      ReadExpectPattern(0x8, 3, 11), WritePattern(0x28, 3, 11),
//      ReadExpectPattern(0x10, 3, 12),WritePattern(0x30, 3, 12),
//      ReadExpectPattern(0x18, 3, 13), WritePattern(0x38, 3, 13))
//    val TLMasterPattern = LazyModule(new VerifTLMasterPattern(mastertxns))
//    test(TLMasterPattern.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
//
//      // Currently just recording requests, only Driver is needed
//      val passInAgent = new TLMasterDriverBasic(c.clock, TLMasterPattern.out)
//      //      val passOutAgent = new TLMasterMonitorBasic(c.clock, TLMasterPattern.out)
//      val simCycles = 100
//
//      c.testIO.run.poke(true.B)
//
//      c.clock.step(simCycles)
//
//      val output = passInAgent.getMonitoredTransactions.toArray[TLBundleA]
//
//      // TODO Add software model here
//      val swoutput = Array(
//        TLUBundleAHelper(opcode = 4.U, address = 0.U),
//        TLUBundleAHelper(opcode = 0.U, address = 0x20.U, data = 10.U),
//        TLUBundleAHelper(opcode = 4.U, address = 0x8.U),
//        TLUBundleAHelper(opcode = 0.U, address = 0x28.U, data = 11.U),
//        TLUBundleAHelper(opcode = 4.U, address = 0x10.U),
//        TLUBundleAHelper(opcode = 0.U, address = 0x30.U, data = 12.U),
//        TLUBundleAHelper(opcode = 4.U, address = 0x18.U),
//        TLUBundleAHelper(opcode = 0.U, address = 0x38.U, data = 13.U))
//
//      assert(outputChecker.checkOutput(output, {t : TLBundleA => (t.opcode.litValue(), t.address.litValue(), t.data.litValue())},
//        swoutput, {t : TLBundleA => (t.opcode.litValue(), t.address.litValue(), t.data.litValue())}))
//    }
//  }

  it should "VerifTL Test Master" ignore {
    val TLCustomMaster = LazyModule(new VerifTLCustomMaster)
    test(TLCustomMaster.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      implicit val params: TLBundleParameters = TLCustomMaster.out.params
      val requestHandler = new TLDriverSlave(c.clock, TLCustomMaster.out, SlaveMemoryState.init(), testResponseWrapper)
      val monitor = new TLMonitor(c.clock, TLCustomMaster.out)
      val simCycles = 100

      c.clock.step(simCycles)

      val outputA = monitor.getMonitoredTransactions().filter(filterA)

      // TODO Add software model here
      val swoutputA = Array(
        Get(addr = 0x0),
        Put(addr = 0x20, data = 10),
        Get(addr = 0x8),
        Put(addr = 0x20, data = 11),
        Get(addr = 0x10),
        Put(addr = 0x30, data = 12),
        Get(addr = 0x18),
        Put(addr = 0x38, data = 13)
      )

      outputA.zip(swoutputA).foreach {
        case (dut_out, sw_out) =>
          assert(dut_out.data == sw_out)
      }
    }
  }
}
