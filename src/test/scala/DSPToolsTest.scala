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

//      val protoTLBundleA = new TLBundleA(standaloneParams)
//      val testA = protoTLBundleA.Lit(_.opcode -> 0.U(3.W), _.param -> 0.U(3.W), _.size -> 2.U(6.W), _.source -> 1.U(1.W),
//        _.address -> 0.U(8.W))
//      val a = c.in.head.a
//      // Poke fails as testA is a hardware and not a chisel type?
//      a.pokePartial(Decoupled(testA))

      // Trying to use helper function with edge
//      val inE = c.inE.head
//      val (valid, testA) = inE.Get(1.U, 0.U, 6.U)
//      println(valid)
      val clientE = c.cliE
      val (valid, testA) = clientE.Get(1.U, 0.U, 6.U)
      println(valid)

//      // Peeking some value as test
//      println("Got here 2")
//
//      val d = c.out.head.d
//      println(d.bits.data)

      assert(true)
    }
  }
}
