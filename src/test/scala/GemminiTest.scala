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
import freechips.rocketchip.config.{Parameters, Config}
import freechips.rocketchip.rocket._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.system.{BaseConfig}
import freechips.rocketchip.subsystem.TileCrossingParamsLike
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import firrtl.AnnotationSeq
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

//case class DummyTileParams(
//  core: CoreParams = RocketCoreParams(),
//  icache: Option[ICacheParams] = None,
//  dcache: Option[DCacheParams] = None,
//  btb: Option[BTBParams] = None,
//  hartId: Int = 0,
//  beuAddr: Option[BigInt] = None,
//  blockerCtrlAddr: Option[BigInt] = None,
//  name: Option[String] = None
//) extends InstantiableTileParams[DummyTile] {
//  def instantiate(crossing: TileCrossingParamsLike, lookup: LookupByHartIdImpl)(implicit p: Parameters): DummyTile = {
//    new DummyTile(this, crossing, lookup)
//  }
//}
//
//class DummyTile private(
//                          val dummyParams: DummyTileParams,
//                          crossing: ClockCrossingType,
//                          lookup: LookupByHartIdImpl,
//                          q: Parameters)
//  extends BaseTile(dummyParams, crossing, lookup, q)
//  //with SourcesExternalNotifications
//{
//  // Private constructor ensures altered LazyModule.p is used implicitly
//  def this(params: DummyTileParams, crossing: TileCrossingParamsLike, lookup: LookupByHartIdImpl)(implicit p: Parameters) =
//    this(params, crossing.crossingType, lookup, p)
//}

class WithDummyTileParams extends Config ((site, here, up) => {
    case TileKey => RocketTileParams
})


class DummyConfig extends Config(
    new WithDummyTileParams ++
    new chipyard.config.AbstractConfig)

class GemminiTest extends FlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = new DummyConfig


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
      stage.execute(Array("-X", "verilog"), ChiselGeneratorAnnotation(() => c.gemmini.module) +: Seq.empty)
      assert(true)
    }
  }
}