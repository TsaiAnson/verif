package verif

import org.scalatest._
import chisel3._
import chisel3.util.Decoupled
import chiseltest._
import designs.{VerifTLPassthroughClient, VerifTLPassthroughClientFuzzer, VerifTLPassthroughClientPattern, VerifTLPassthroughManager, VerifTLStandaloneBlock}
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, VerilatorBackendAnnotation, WriteVcdAnnotation}
import dspblocks.{PassthroughParams, TLPassthrough, TLStandaloneBlock}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.system.BaseConfig
import freechips.rocketchip.tilelink._
import chisel3.experimental.BundleLiterals._
import freechips.rocketchip.subsystem.WithoutTLMonitors
import freechips.rocketchip.util._

case object MyBundleData extends DataKey[UInt]("data")
case class MyBundleDataField(width: Int) extends SimpleBundleField(MyBundleData)(Output(UInt(width.W)), 0.U)

class DSPToolsTest extends FlatSpec with ChiselScalatestTester {
//  implicit val p: Parameters = Parameters.empty.asInstanceOf[Parameters]
  //  implicit val p: Parameters = (new BaseConfig).toInstance
  implicit val p: Parameters = new WithoutTLMonitors

  def standaloneParams = TLBundleParameters(addressBits = 64, dataBits = 64, sourceBits = 1,
    sinkBits = 1, sizeBits = 6,
    echoFields = Seq(), requestFields = Seq(), responseFields = Seq(),
    hasBCE = false)

  it should "VerifTL Test Manager" in {
    val TLPassthrough = LazyModule(new VerifTLPassthroughManager with VerifTLStandaloneBlock)
    test(TLPassthrough.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      val passInAgent = new TLManagerDriverBasic(c.clock, TLPassthrough.in)
      val passOutAgent = new TLManagerMonitorBasic(c.clock, TLPassthrough.in)
      val simCycles = 100

      val inputTransactions = Seq(
        // Read back the values in registers 0x00, 0x08, 0x10, 0x18
        VerifTLAChannel(opcode = 4.U, address = 0.U),
        VerifTLAChannel(opcode = 4.U, address = 0x08.U),
        VerifTLAChannel(opcode = 4.U, address = 0x10.U),
        VerifTLAChannel(opcode = 4.U, address = 0x18.U),
        // Write values into registers 0x00, 0x08, 0x10, 0x18
        VerifTLAChannel(opcode = 0.U, address = 0.U, data = 0.U),
        VerifTLAChannel(opcode = 0.U, address = 0x08.U, data = 1.U),
        VerifTLAChannel(opcode = 0.U, address = 0x10.U, data = 2.U),
        VerifTLAChannel(opcode = 0.U, address = 0x18.U, data = 3.U),
        // Read back the values in registers 0x00, 0x08, 0x10, 0x18
        VerifTLAChannel(opcode = 4.U, address = 0.U),
        VerifTLAChannel(opcode = 4.U, address = 0x08.U),
        VerifTLAChannel(opcode = 4.U, address = 0x10.U),
        VerifTLAChannel(opcode = 4.U, address = 0x18.U)
      )

      passInAgent.push(inputTransactions)
      c.clock.step(simCycles)

      val output = passOutAgent.getMonitoredTransactions.toArray[VerifTLDChannel]

      // TODO Add software model here
      val swoutput = Array(
        VerifTLDChannel(data = 10.U),
        VerifTLDChannel(data = 11.U),
        VerifTLDChannel(data = 12.U),
        VerifTLDChannel(data = 13.U),
        VerifTLDChannel(data = 0.U),
        VerifTLDChannel(data = 1.U),
        VerifTLDChannel(data = 2.U),
        VerifTLDChannel(data = 3.U),
        VerifTLDChannel(data = 0.U),
        VerifTLDChannel(data = 1.U),
        VerifTLDChannel(data = 2.U),
        VerifTLDChannel(data = 3.U))

      assert(outputChecker.checkOutput(output, {t : VerifTLDChannel => t.data.litValue()},
        swoutput, {t : VerifTLDChannel => t.data.litValue()}))
    }
  }

  it should "VerifTL Test Client Fuzzer" in {
    val TLPassthrough = LazyModule(new VerifTLPassthroughClientFuzzer with VerifTLStandaloneBlock)
    test(TLPassthrough.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      // Currently just recording requests, only Driver is needed
      val passInAgent = new TLClientDriverBasic(c.clock, TLPassthrough.out)
      //      val passOutAgent = new TLClientMonitorBasic(c.clock, TLPassthrough.out)
      val simCycles = 100

      c.clock.step(simCycles)

      val output = passInAgent.getMonitoredTransactions.toArray[VerifTLAChannel]

      // Sanity test to make sure that driver/monitor is correctly getting requests
      assert(output.length == 30)

      for (out <- output) {
        println(out.getElements)
      }
    }
  }

  it should "VerifTL Test Client Pattern" in {
    // Currently hardcoded write values as Patterns does not support dependent patterns
    // (e.g. write a value that was read in earlier pattern)
    val clienttxns = Seq(ReadExpectPattern(0, 3, 10), WritePattern(0x20, 3, 10),
      ReadExpectPattern(0x8, 3, 11), WritePattern(0x28, 3, 11),
      ReadExpectPattern(0x10, 3, 12),WritePattern(0x30, 3, 12),
      ReadExpectPattern(0x18, 3, 13), WritePattern(0x38, 3, 13))
    val TLPassthrough = LazyModule(new VerifTLPassthroughClientPattern(clienttxns) with VerifTLStandaloneBlock)
    test(TLPassthrough.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      // Currently just recording requests, only Driver is needed
      val passInAgent = new TLClientDriverBasic(c.clock, TLPassthrough.out)
      //      val passOutAgent = new TLClientMonitorBasic(c.clock, TLPassthrough.out)
      val simCycles = 100

      c.testIO.run.poke(true.B)

      c.clock.step(simCycles)

      val output = passInAgent.getMonitoredTransactions.toArray[VerifTLAChannel]

      // TODO Add software model here
      val swoutput = Array(
        VerifTLAChannel(opcode = 4.U, address = 0.U),
        VerifTLAChannel(opcode = 0.U, address = 0x20.U, data = 10.U),
        VerifTLAChannel(opcode = 4.U, address = 0x8.U),
        VerifTLAChannel(opcode = 0.U, address = 0x28.U, data = 11.U),
        VerifTLAChannel(opcode = 4.U, address = 0x10.U),
        VerifTLAChannel(opcode = 0.U, address = 0x30.U, data = 12.U),
        VerifTLAChannel(opcode = 4.U, address = 0x18.U),
        VerifTLAChannel(opcode = 0.U, address = 0x38.U, data = 13.U))

      assert(outputChecker.checkOutput(output, {t : VerifTLAChannel => (t.opcode.litValue(), t.address.litValue(), t.data.litValue())},
        swoutput, {t : VerifTLAChannel => (t.opcode.litValue(), t.address.litValue(), t.data.litValue())}))
    }
  }

  it should "VerifTL Test Client" in {
    val TLPassthrough = LazyModule(new VerifTLPassthroughClient with VerifTLStandaloneBlock)
    test(TLPassthrough.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      // Currently just recording requests, only Driver is needed
      val passInAgent = new TLClientDriverBasic(c.clock, TLPassthrough.out)
//      val passOutAgent = new TLClientMonitorBasic(c.clock, TLPassthrough.out)
      val simCycles = 80

      c.clock.step(simCycles)

      val output = passInAgent.getMonitoredTransactions.toArray[VerifTLAChannel]

      // TODO Add software model here
      val swoutput = Array(
        VerifTLAChannel(opcode = 4.U, address = 0.U),
        VerifTLAChannel(opcode = 0.U, address = 0x20.U, data = 10.U),
        VerifTLAChannel(opcode = 4.U, address = 0x8.U),
        VerifTLAChannel(opcode = 0.U, address = 0x28.U, data = 11.U),
        VerifTLAChannel(opcode = 4.U, address = 0x10.U),
        VerifTLAChannel(opcode = 0.U, address = 0x30.U, data = 12.U),
        VerifTLAChannel(opcode = 4.U, address = 0x18.U),
        VerifTLAChannel(opcode = 0.U, address = 0x38.U, data = 13.U))

      for (out <- output) {
        println(out.getElements)
      }

      assert(outputChecker.checkOutput(output, {t : VerifTLAChannel => (t.opcode.litValue(), t.address.litValue(), t.data.litValue())},
        swoutput, {t : VerifTLAChannel => (t.opcode.litValue(), t.address.litValue(), t.data.litValue())}))
    }
  }
}
