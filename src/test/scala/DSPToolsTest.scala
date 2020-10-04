package verif

import org.scalatest._
import chisel3._
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

class DSPToolsTest extends FlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = Parameters.empty.asInstanceOf[Parameters]
  //  implicit val p: Parameters = (new BaseConfig).toInstance

  def standaloneParams = TLBundleParameters(addressBits = 64, dataBits = 64, sourceBits = 1,
    sinkBits = 1, sizeBits = 6,
    echoFields = Seq(), requestFields = Seq(), responseFields = Seq(),
    hasBCE = false)

  it should "DSPTools Test" in {
    val TLPassthrough = LazyModule(new VerifTLPassthrough with VerifTLStandaloneBlock)
    test(TLPassthrough.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      println("Got here")

      val protoTLBundleA = new TLBundleA(standaloneParams)

      val testA = protoTLBundleA.Lit(_.opcode -> 0.U(3.W), _.param -> 0.U, _.size -> 2.U, _.source -> 1.U,
        _.address -> 0.U, _.mask -> 0xff.U)

      var a = c.in.head.a

      a.bits.pokePartial(testA)

//      a.bits.opcode.poke(0.U)
//      a.bits.param.poke(0.U)
//      a.bits.size.poke(2.U)
//      a.bits.source.poke(1.U)
//      // Poking some value for address
//      a.bits.address.poke(0.U)
//      a.bits.mask.poke(0xff.U)
//      a.bits.data.poke(0.U)

      // Peeking some value as test
      println("Got here 2")
      var d = c.out.head.d
      println(d.bits.data)

      assert(true)
    }
  }
}
