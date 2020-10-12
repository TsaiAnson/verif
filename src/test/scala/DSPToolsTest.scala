package verif

import org.scalatest._
import chisel3._
import chisel3.util.Decoupled
import chiseltest._
import designs.{VerifTLPassthroughClient, VerifTLPassthroughManager, VerifTLStandaloneBlock}
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}
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

      //  val testBundleMap = new BundleMap(Seq(MyBundleDataField(1)))
      // // Poking with bundle literals is not working, leaving here for reference
      // val protoTLBundleA = new TLBundleA(standaloneParams)
      // println(TLPassthrough.in.params)
      // val testA = protoTLBundleA.Lit(_.opcode -> 0.U(3.W), _.param -> 0.U(3.W), _.size -> 2.U(6.W), _.source -> 1.U(1.W),
      //   _.address -> 0.U(8.W), _.user -> testBundleMap, _.echo -> testBundleMap,
      //   _.address -> 0.U(8.W),
      //   _.mask -> 0xfff.U, _.data -> 0.U, _.corrupt -> false.B)
      // // Poke fails as testA is a hardware and not a chisel type?
      // a.pokePartial(Decoupled(testA))
    }
  }

  it should "VerifTL Test Client" in {
    val TLPassthrough = LazyModule(new VerifTLPassthroughClient with VerifTLStandaloneBlock)
    test(TLPassthrough.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      // Currently just recording requests, only Driver is needed
      val passInAgent = new TLClientDriverBasic(c.clock, TLPassthrough.out)
//      val passOutAgent = new TLClientMonitorBasic(c.clock, TLPassthrough.out)
      val simCycles = 20

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
}
