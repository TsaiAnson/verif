package cosim

import org.scalatest.flatspec.AnyFlatSpec

import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation, CachingAnnotation, WriteVcdAnnotation}
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.diplomacy.{LazyModule}
import freechips.rocketchip.tile.{RoCCCommand}
import gemmini._
import java.io.File
import scala.collection.JavaConverters._
import scala.sys.process._
import verif._

import com.verif._

class GemminiTest extends AnyFlatSpec with CosimTester with ChiselScalatestTester {
  implicit val p: Parameters = VerifTestUtils.getVerifParameters()

  val dut = LazyModule(
    new VerifRoCCStandaloneWrapper(
      /*** SET YOUR CONFIGURATION FOR COSIM HERE ***/
      () => new Gemmini(GemminiConfigs.defaultConfig.copy(use_dedicated_tl_port = true,
    meshRows = 4, meshColumns = 4, rob_full_entries = 4)),
    beatBytes = 16,
    addSinks = 1
  ))

  val simPath = "spike"
  val simArgs = Seq("--extension=gemmini")

  it should "Elaborate for parameters and build gemmini-rocc-tests" in {
    ChiselStage.elaborate(dut.module)
    assert(Process("./build.sh", new File(s"${cosimTestDetails.sbtRoot.get}/generators/gemmini/software/gemmini-rocc-tests/"), System.getenv().asScala.toSeq:_*).! == 0)
  }

  it should "Run user test target" in {
    println(simTarget)
    test(dut.module).withAnnotations(Seq(VerilatorBackendAnnotation, CachingAnnotation, WriteVcdAnnotation)) { c =>
      val commandPipe = () => new RoCCCommandPipeDriver("RoCCCommandPipe", c.clock, c.io.cmd)
      val fencePipe = () => new FencePipeConnector("GemminiFenceReqPipe", "GemminiFenceRespPipe", c.clock, c.io)
      val tlPipe = () => new TLPipeConnector("TLAPipe", "TLDPipe", c.clock, c.tlOut(0));

      val manager = new CosimManager(simPath, Seq(commandPipe, fencePipe, tlPipe), c.clock)
      manager.run(simArgs, simTarget, x => x == 0)
    }
  }

  it should "Run mvin_mvout-baremetal" in {
    val simTarget = "generators/gemmini/software/gemmini-rocc-tests/build/bareMetalC/mvin_mvout-baremetal"

    test(dut.module).withAnnotations(Seq(VerilatorBackendAnnotation, CachingAnnotation, WriteVcdAnnotation)) { c =>
      val commandPipe = () => new RoCCCommandPipeDriver("RoCCCommandPipe", c.clock, c.io.cmd)
      val fencePipe = () => new FencePipeConnector("GemminiFenceReqPipe", "GemminiFenceRespPipe", c.clock, c.io)
      val tlPipe = () => new TLPipeConnector("TLAPipe", "TLDPipe", c.clock, c.tlOut(0));

      val manager = new CosimManager(simPath, Seq(commandPipe, fencePipe, tlPipe), c.clock)
      manager.run(simArgs, simTarget, x => x == 0)
    }
  }

  it should "Run matrix_add-baremetal" in {
    val simTarget = "generators/gemmini/software/gemmini-rocc-tests/build/bareMetalC/matrix_add-baremetal"

    test(dut.module).withAnnotations(Seq(VerilatorBackendAnnotation, CachingAnnotation, WriteVcdAnnotation)) { c =>
      val commandPipe = () => new RoCCCommandPipeDriver("RoCCCommandPipe", c.clock, c.io.cmd)
      val fencePipe = () => new FencePipeConnector("GemminiFenceReqPipe", "GemminiFenceRespPipe", c.clock, c.io)
      val tlPipe = () => new TLPipeConnector("TLAPipe", "TLDPipe", c.clock, c.tlOut(0));

      val manager = new CosimManager(simPath, Seq(commandPipe, fencePipe, tlPipe), c.clock)
      manager.run(simArgs, simTarget, x => x == 0)
    }
  }

  it should "Run matmul-baremetal" in {
    val simTarget = "generators/gemmini/software/gemmini-rocc-tests/build/bareMetalC/matmul-baremetal"

    test(dut.module).withAnnotations(Seq(VerilatorBackendAnnotation, CachingAnnotation, WriteVcdAnnotation)) { c =>
      val commandPipe = () => new RoCCCommandPipeDriver("RoCCCommandPipe", c.clock, c.io.cmd)
      val fencePipe = () => new FencePipeConnector("GemminiFenceReqPipe", "GemminiFenceRespPipe", c.clock, c.io)
      val tlPipe = () => new TLPipeConnector("TLAPipe", "TLDPipe", c.clock, c.tlOut(0));

      val manager = new CosimManager(simPath, Seq(commandPipe, fencePipe, tlPipe), c.clock)
      manager.run(simArgs, simTarget, x => x == 0)
    }
  }
}
