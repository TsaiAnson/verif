package verif

import org.scalatest._
import chisel3._
import chisel3.util.Decoupled
import chiseltest._
import designs._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.system.BaseConfig
import freechips.rocketchip.tilelink._
import chisel3.experimental.BundleLiterals._
import freechips.rocketchip.subsystem.WithoutTLMonitors
import freechips.rocketchip.util._

//case object MyBundleData extends DataKey[UInt]("data")
//case class MyBundleDataField(width: Int) extends SimpleBundleField(MyBundleData)(Output(UInt(width.W)), 0.U)

class DSPToolsTest extends FlatSpec with ChiselScalatestTester {
//  implicit val p: Parameters = Parameters.empty.asInstanceOf[Parameters]
  //  implicit val p: Parameters = (new BaseConfig).toInstance
  implicit val p: Parameters = new WithoutTLMonitors

  it should "VerifTL Test Slave" in {
    val TLRegBankSlave = LazyModule(new VerifTLRegBankSlave with VerifTLStandaloneBlock)
    test(TLRegBankSlave.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      val passInAgent = new TLSlaveDriverBasic(c.clock, TLRegBankSlave.in)
      val passOutAgent = new TLSlaveMonitorBasic(c.clock, TLRegBankSlave.in)
      val simCycles = 100

      val inputTransactions = Seq(
        // Read back the values in registers 0x00, 0x08, 0x10, 0x18
        Get(addr = 0.U),
        Get(addr = 0x08.U),
        Get(addr = 0x10.U),
        Get(addr = 0x18.U),
        // Write values into registers 0x00, 0x08, 0x10, 0x18
        FullPut(addr = 0.U, data = 0.U),
        FullPut(addr = 0x08.U, data = 1.U),
        FullPut(addr = 0x10.U, data = 2.U),
        FullPut(addr = 0x18.U, data = 3.U),
        // Read back the values in registers 0x00, 0x08, 0x10, 0x18
        Get(addr = 0.U),
        Get(addr = 0x08.U),
        Get(addr = 0x10.U),
        Get(addr = 0x18.U)
      )

      passInAgent.push(inputTransactions)
      c.clock.step(simCycles)

      val output = passOutAgent.getMonitoredTransactions.toArray

      // TODO Add software model here
      val swoutput = Array(
        AccessAckData(data = 0.U(64.W)),
        AccessAckData(data = 0.U(64.W)),
        AccessAckData(data = 0.U(64.W)),
        AccessAckData(data = 0.U(64.W)),
        AccessAckData(data = 0.U(64.W)),
        AccessAckData(data = 1.U(64.W)),
        AccessAckData(data = 2.U(64.W)),
        AccessAckData(data = 3.U(64.W)),
        AccessAckData(data = 0.U(64.W)),
        AccessAckData(data = 1.U(64.W)),
        AccessAckData(data = 2.U(64.W)),
        AccessAckData(data = 3.U(64.W)))

      assert(outputChecker.checkOutput(output, {t : TLTransaction => t},
        swoutput, {t : TLTransaction => t}))
    }
  }

  it should "VerifTL Test Master Fuzzer" in {
    val TLMasterFuzzer = LazyModule(new VerifTLMasterFuzzer with VerifTLStandaloneBlock)
    test(TLMasterFuzzer.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      // Currently just recording requests, only Driver is needed
      val passInAgent = new TLMasterDriverBasic(c.clock, TLMasterFuzzer.out)
      //      val passOutAgent = new TLMasterMonitorBasic(c.clock, TLPassthrough.out)
      val simCycles = 100

      c.clock.step(simCycles)

      val output = passInAgent.getMonitoredTransactions.toArray

      // Sanity test to make sure that driver/monitor is correctly getting requests
      assert(output.length == 30)

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
//    val TLMasterPattern = LazyModule(new VerifTLMasterPattern(mastertxns) with VerifTLStandaloneBlock)
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

  it should "VerifTL Test Master" in {
    val TLCustomMaster = LazyModule(new VerifTLCustomMaster with VerifTLStandaloneBlock)
    test(TLCustomMaster.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      // Currently just recording requests, only Driver is needed
      val passInAgent = new TLMasterDriverBasic(c.clock, TLCustomMaster.out)
//      val passOutAgent = new TLMasterMonitorBasic(c.clock, TLPassthrough.out)
      val simCycles = 80

      c.clock.step(simCycles)

      val output = passInAgent.getMonitoredTransactions.toArray

      // TODO Add software model here
      val swoutput = Array(
        Get(addr = 0.U(64.W)),
        FullPut(addr = 0x20.U(64.W), data = 10.U(64.W)),
        Get(addr = 0x8.U(64.W)),
        FullPut(addr = 0x28.U(64.W), data = 11.U(64.W)),
        Get(addr = 0x10.U(64.W)),
        FullPut(addr = 0x30.U(64.W), data = 12.U(64.W)),
        Get(addr = 0x18.U(64.W)),
        FullPut(addr = 0x38.U(64.W), data = 13.U(64.W)))

//      for (out <- output) {
//        println(out.getElements)
//      }
      assert(outputChecker.checkOutput(output, {t : TLTransaction => t},
        swoutput, {t : TLTransaction => t}))
    }
  }
}
