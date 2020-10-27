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

class DSPToolsTest extends FlatSpec with ChiselScalatestTester with VerifTLBase{
//  implicit val p: Parameters = Parameters.empty.asInstanceOf[Parameters]
  //  implicit val p: Parameters = (new BaseConfig).toInstance
  implicit val p: Parameters = new WithoutTLMonitors

  it should "VerifTL Test Manager" in {
    val TLPassthrough = LazyModule(new VerifTLRegBankManager with VerifTLStandaloneBlock)
    test(TLPassthrough.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      val passInAgent = new TLManagerDriverBasic(c.clock, TLPassthrough.in)
      val passOutAgent = new TLManagerMonitorBasic(c.clock, TLPassthrough.in)
      val simCycles = 100

      val inputTransactions = Seq(
        // Read back the values in registers 0x00, 0x08, 0x10, 0x18
        TLUBundleAHelper(opcode = 4.U, address = 0.U),
        TLUBundleAHelper(opcode = 4.U, address = 0x08.U),
        TLUBundleAHelper(opcode = 4.U, address = 0x10.U),
        TLUBundleAHelper(opcode = 4.U, address = 0x18.U),
        // Write values into registers 0x00, 0x08, 0x10, 0x18
        TLUBundleAHelper(opcode = 0.U, address = 0.U, data = 0.U),
        TLUBundleAHelper(opcode = 0.U, address = 0x08.U, data = 1.U),
        TLUBundleAHelper(opcode = 0.U, address = 0x10.U, data = 2.U),
        TLUBundleAHelper(opcode = 0.U, address = 0x18.U, data = 3.U),
        // Read back the values in registers 0x00, 0x08, 0x10, 0x18
        TLUBundleAHelper(opcode = 4.U, address = 0.U),
        TLUBundleAHelper(opcode = 4.U, address = 0x08.U),
        TLUBundleAHelper(opcode = 4.U, address = 0x10.U),
        TLUBundleAHelper(opcode = 4.U, address = 0x18.U)
      )

      passInAgent.push(inputTransactions)
      c.clock.step(simCycles)

      val output = passOutAgent.getMonitoredTransactions.toArray[TLBundleD]

      // TODO Add software model here
      val swoutput = Array(
        TLUBundleDHelper(data = 10.U),
        TLUBundleDHelper(data = 11.U),
        TLUBundleDHelper(data = 12.U),
        TLUBundleDHelper(data = 13.U),
        TLUBundleDHelper(data = 0.U),
        TLUBundleDHelper(data = 1.U),
        TLUBundleDHelper(data = 2.U),
        TLUBundleDHelper(data = 3.U),
        TLUBundleDHelper(data = 0.U),
        TLUBundleDHelper(data = 1.U),
        TLUBundleDHelper(data = 2.U),
        TLUBundleDHelper(data = 3.U))

      assert(outputChecker.checkOutput(output, {t : TLBundleD => t.data.litValue()},
        swoutput, {t : TLBundleD => t.data.litValue()}))
    }
  }

  it should "VerifTL Test Client Fuzzer" in {
    val TLPassthrough = LazyModule(new VerifTLClientFuzzer with VerifTLStandaloneBlock)
    test(TLPassthrough.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      // Currently just recording requests, only Driver is needed
      val passInAgent = new TLClientDriverBasic(c.clock, TLPassthrough.out)
      //      val passOutAgent = new TLClientMonitorBasic(c.clock, TLPassthrough.out)
      val simCycles = 100

      c.clock.step(simCycles)

      val output = passInAgent.getMonitoredTransactions.toArray[TLBundleA]

      // Sanity test to make sure that driver/monitor is correctly getting requests
      assert(output.length == 30)

//      for (out <- output) {
//        println(out.getElements)
//      }
    }
  }

//  it should "VerifTL Test Client Pattern" in {
//    // Currently hardcoded write values as Patterns does not support dependent patterns
//    // (e.g. write a value that was read in earlier pattern)
//    val clienttxns = Seq(ReadExpectPattern(0, 3, 10), WritePattern(0x20, 3, 10),
//      ReadExpectPattern(0x8, 3, 11), WritePattern(0x28, 3, 11),
//      ReadExpectPattern(0x10, 3, 12),WritePattern(0x30, 3, 12),
//      ReadExpectPattern(0x18, 3, 13), WritePattern(0x38, 3, 13))
//    val TLPassthrough = LazyModule(new VerifTLClientPattern(clienttxns) with VerifTLStandaloneBlock)
//    test(TLPassthrough.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
//
//      // Currently just recording requests, only Driver is needed
//      val passInAgent = new TLClientDriverBasic(c.clock, TLPassthrough.out)
//      //      val passOutAgent = new TLClientMonitorBasic(c.clock, TLPassthrough.out)
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

  it should "VerifTL Test Client" in {
    val TLPassthrough = LazyModule(new VerifTLCustomClient with VerifTLStandaloneBlock)
    test(TLPassthrough.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      // Currently just recording requests, only Driver is needed
      val passInAgent = new TLClientDriverBasic(c.clock, TLPassthrough.out)
//      val passOutAgent = new TLClientMonitorBasic(c.clock, TLPassthrough.out)
      val simCycles = 80

      c.clock.step(simCycles)

      val output = passInAgent.getMonitoredTransactions.toArray[TLBundleA]

      // TODO Add software model here
      val swoutput = Array(
        TLUBundleAHelper(opcode = 4.U, address = 0.U),
        TLUBundleAHelper(opcode = 0.U, address = 0x20.U, data = 10.U),
        TLUBundleAHelper(opcode = 4.U, address = 0x8.U),
        TLUBundleAHelper(opcode = 0.U, address = 0x28.U, data = 11.U),
        TLUBundleAHelper(opcode = 4.U, address = 0x10.U),
        TLUBundleAHelper(opcode = 0.U, address = 0x30.U, data = 12.U),
        TLUBundleAHelper(opcode = 4.U, address = 0x18.U),
        TLUBundleAHelper(opcode = 0.U, address = 0x38.U, data = 13.U))

//      for (out <- output) {
//        println(out.getElements)
//      }

      assert(outputChecker.checkOutput(output, {t : TLBundleA => (t.opcode.litValue(), t.address.litValue(), t.data.litValue())},
        swoutput, {t : TLBundleA => (t.opcode.litValue(), t.address.litValue(), t.data.litValue())}))
    }
  }
}
