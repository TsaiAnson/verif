package verif

import org.scalatest._
import chisel3._
import chiseltest._
import chisel3.util._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}
import freechips.rocketchip.config.Parameters
import rocketdsptools.TLPassthroughBlock

class DSPToolsTest extends FlatSpec with ChiselScalatestTester {

  it should "DSPTools Test" in {
    test(new TLPassthroughBlock).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>



      assert(true)
    }
  }
}