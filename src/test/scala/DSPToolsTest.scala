package verif

import org.scalatest._
import chisel3._
import chiseltest._
import chisel3.util._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.system.BaseConfig
import dspblocks.{PassthroughParams, TLPassthrough, TLStandaloneBlock}
import freechips.rocketchip.diplomacy.LazyModule


class DSPToolsTest extends FlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = Parameters.empty.asInstanceOf[Parameters]
  //  implicit val p: Parameters = (new BaseConfig).toInstance

  it should "DSPTools Test" in {
    val params = PassthroughParams(depth = 8)
    test(LazyModule(new TLPassthrough(params) with TLStandaloneBlock).module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>



      assert(true)
    }
  }
}