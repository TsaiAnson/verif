package verif

import org.scalatest.flatspec.AnyFlatSpec

import designs._
import cosim._
import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation, CachingAnnotation, WriteVcdAnnotation}
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.tile.{RoCCCommand}
import java.io.{ByteArrayOutputStream, FileInputStream, FileOutputStream, PrintStream, BufferedReader, InputStreamReader}
import java.util.stream.Collectors
import org.scalatest.matchers._
import scala.sys.process._
import scala.reflect.io.File
import freechips.rocketchip.diplomacy.{LazyModule}
import freechips.rocketchip.tile.{RoCCCommand, OpcodeSet}
import gemmini._

import com.verif._

class CosimTest extends AnyFlatSpec with CosimTester with ChiselScalatestTester {
  implicit val p: Parameters = VerifTestUtils.getVerifParameters()

  val dut = LazyModule(
    new VerifRoCCStandaloneWrapper(
      /*** SET YOUR CONFIGURATION FOR COSIM HERE ***/
      () => new Gemmini(OpcodeSet.custom3, GemminiConfigs.defaultConfig.copy(use_dedicated_tl_port = true,
    meshRows = 4, meshColumns = 4, rob_entries = 4)),
  beatBytes = 16,
  addSinks = 1
  ))

  val simPath = "spike"
  val simArgs = Seq("--extension=gemmini")

  it should "elaborate for parameters" in {
    test(dut.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      assert(true)
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
