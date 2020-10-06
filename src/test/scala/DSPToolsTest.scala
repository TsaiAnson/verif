package verif

import org.scalatest._
import chisel3._
import chisel3.util.Decoupled
import chiseltest._
import designs.{VerifTLPassthrough, VerifTLStandaloneBlock}
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

  val testBundleMap = new BundleMap(Seq(MyBundleDataField(1)))

  it should "DSPTools Test" in {
    val TLPassthrough = LazyModule(new VerifTLPassthrough with VerifTLStandaloneBlock)
    test(TLPassthrough.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      val a = TLPassthrough.in.a

      a.bits.opcode.poke(0.U)
      a.bits.param.poke(0.U)
      a.bits.address.poke(0x10.U)
      a.bits.size.poke(2.U)
      a.bits.source.poke(1.U)
      a.bits.mask.poke(0xfff.U)
      a.bits.data.poke(0.U)

      a.valid.poke(true.B)

      while (a.ready.peek().litToBoolean) {
        c.clock.step(1)
      }
      c.clock.step(1)
      a.valid.poke(false.B)

      // Checking for response
      val d = TLPassthrough.in.d
      d.ready.poke(true.B)
      while (!d.valid.peek().litToBoolean) {
        c.clock.step(1)
      }
      // Read output
      println(s"opcode ${d.bits.opcode.peek()}")
      println(s"param ${d.bits.param.peek()}")
      println(s"size ${d.bits.size.peek()}")
      println(s"source ${d.bits.source.peek()}")
      println(s"sink ${d.bits.sink.peek()}")
      println(s"data ${d.bits.data.peek()}")
      println(s"corrupt ${d.bits.corrupt.peek()}")

      c.clock.step(1)
      d.ready.poke(false.B)
      c.clock.step(5)

      a.bits.opcode.poke(4.U)
      a.bits.param.poke(0.U)
      a.bits.address.poke(0x10.U)
      a.bits.size.poke(2.U)
      a.bits.source.poke(1.U)
      a.bits.mask.poke(0xfff.U)
      a.bits.data.poke(0.U)

      a.valid.poke(true.B)

      while (a.ready.peek().litToBoolean) {
        c.clock.step(1)
      }
      c.clock.step(1)
      a.valid.poke(false.B)

      // Checking for response
      d.ready.poke(true.B)
      while (!d.valid.peek().litToBoolean) {
        c.clock.step(1)
      }
      // Read output
      println(s"opcode ${d.bits.opcode.peek()}")
      println(s"param ${d.bits.param.peek()}")
      println(s"size ${d.bits.size.peek()}")
      println(s"source ${d.bits.source.peek()}")
      println(s"sink ${d.bits.sink.peek()}")
      println(s"data ${d.bits.data.peek()}")
      println(s"corrupt ${d.bits.corrupt.peek()}")

      c.clock.step(1)
      d.ready.poke(false.B)
      c.clock.step(5)

      // Trying to use helper function with edge
//      val inE = c.inE
//      val (valid, testA) = inE.Get(1.U, 0.U, 6.U)
//      println(valid)
//      val clientE = c.cliE
//      val (valid, testA) = clientE.Get(1.U, 0.U, 6.U)
//      println(valid)

//      // Peeking some value as test
//      println("Got here 2")
//
//      val d = c.out.head.d
//      println(d.bits.data)


      // // Poking with bundle literals is not working, leaving here for reference
      // val protoTLBundleA = new TLBundleA(standaloneParams)
      // println(TLPassthrough.in.params)
      // val testA = protoTLBundleA.Lit(_.opcode -> 0.U(3.W), _.param -> 0.U(3.W), _.size -> 2.U(6.W), _.source -> 1.U(1.W),
      //   _.address -> 0.U(8.W), _.user -> testBundleMap, _.echo -> testBundleMap,
      //   _.address -> 0.U(8.W),
      //   _.mask -> 0xfff.U, _.data -> 0.U, _.corrupt -> false.B)
      // // Poke fails as testA is a hardware and not a chisel type?
      // a.pokePartial(Decoupled(testA))

      assert(true)
    }
  }
}
