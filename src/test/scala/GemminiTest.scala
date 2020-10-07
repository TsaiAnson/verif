package verif

import org.scalatest._
import chisel3._
import chiseltest._
import chisel3.util._
import chisel3.stage.ChiselGeneratorAnnotation
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation, TreadleBackendAnnotation, WriteVcdAnnotation}
import chipyard.config.{AbstractConfig}
import testchipip.{TLHelper}
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import firrtl.AnnotationSeq
import gemmini.GemminiConfigs.defaultConfig
import gemmini._

class GemminiManagerNode(beatBytes: Int)(implicit p: Parameters) extends LazyModule {
  val device = new SimpleDevice("gemminimanagernode", Seq("gemmini"))
  val node = TLRegisterNode(
    address = Seq(AddressSet(0x0, 0xfff)),
    device = device,
    beatBytes = beatBytes,
    concurrency = 1)
  //val node = TLHelper.makeManagerNode(beatBytes, TLManagerParameters(
  //  address = Seq(AddressSet(0x0, 0xffff))))
  lazy val module = new LazyModuleImp(this) {
    val (tl, edge) = node.in(0)
  }
}


class GemminiTest extends FlatSpec with ChiselScalatestTester {
  //case TileKey => None
  implicit val p: Parameters = new freechips.rocketchip.system.BaseConfig
  case TileKey =>
  //implicit val p: Parameters = new AbstractConfig

  it should "Elaborate Gemmini" in {
    test(new MultiIOModule {
      val xbar = LazyModule(new TLXbar)
      val manager = LazyModule(new GemminiManagerNode(16))
      val gemmini = LazyModule(new Gemmini(OpcodeSet.custom3, GemminiConfigs.defaultConfig))
      xbar.node := gemmini.tlNode
      manager.node := xbar.node
      // Define driver and monitor here
      // Hookup driver and monitor to Gemmini here
    }).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val stage = new chisel3.stage.ChiselStage()
      stage.execute(Array("-X", "sverilog"), ChiselGeneratorAnnotation(() => c.gemmini.module) +: Seq.empty)
      assert(true)
    }
  }
}