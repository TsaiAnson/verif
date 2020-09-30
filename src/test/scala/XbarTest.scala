package verif

import org.scalatest._
import chisel3._
import chiseltest._
import chisel3.util._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.tilelink.TLXbar
import freechips.rocketchip.diplomacy.LazyModule


class XbarTest extends FlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = Parameters.empty.asInstanceOf[Parameters]

  it should "Elaborate XBar" in {
    test(new MultiIOModule {
      val xbar = LazyModule(new TLXbar)
      // Define driver and monitor here
      // Hookup xbar to driver and monitor here
    }).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      assert(true)
    }
  }
}