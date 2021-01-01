package verif

import org.scalatest.flatspec.AnyFlatSpec

import designs._
import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.tile.{RoCCCommand}
import java.io.{ByteArrayOutputStream, FileInputStream, FileOutputStream, PrintStream, BufferedReader, InputStreamReader}
import java.util.stream.Collectors
import org.scalatest.matchers._
import scala.sys.process._
import scala.reflect.io.File

import com.verif._


class CosimTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = VerifTestUtils.getVerifParameters()

  it should "Named Pipe Test" in {
    test(new MultiIOModule {}).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      val fifoName = "TestFIFO"
      val fifoPath = s"${File(".").toAbsolute}/test_run_dir/should_Named_Pipe_Test/"
      val fifo = s"$fifoPath$fifoName"

      val cmd = RoCCProtos.RoCCCommand.newBuilder()
                .setRs2(((BigInt(2) << 48) + (BigInt(1) << 32)).toLong)
                .setInst(
                  RoCCProtos.RoCCInstruction.newBuilder()
                    .setFunct(2))
                .build()

      val write = new Thread {
        override def run: Unit = {
          val out = new FileOutputStream(fifo)
          cmd.writeDelimitedTo(out)
        }
      }

      val read = new Thread {
        override def run: Unit = {
          val in = new FileInputStream(fifo)
          val message = com.verif.RoCCProtos.RoCCCommand.parseDelimitedFrom(in)
          println(message)
        }
      }

      println("Creating FIFO")
      Runtime.getRuntime().exec(s"mkfifo ${fifo}");
      Thread.sleep(1000)

      println("Starting Reader and Writer")
      write.start()
      read.start()

      Thread.sleep(1000)

      println("Stopping Reader and Writer")
      write.stop()
      read.stop()

      println("Deleting FIFO")
      Runtime.getRuntime().exec(s"rm ${fifo}");

      assert(true)
    }
  }

  it should "Run Spike" in {
    val simPath = "/home/rlund/adept/chipyard/esp-tools-install/bin/spike"
    val simArgs = Seq("--extension=gemmini")
    val simTarget = "/home/rlund/adept/chipyard/generators/gemmini/software/gemmini-rocc-tests/build/bareMetalC/mvin_mvout-baremetal"

    val out = VerifCosimTestUtils.runCommand(Seq(simPath, simArgs.mkString(" "), simTarget))

    println("-- Output of running Spike --")
    println(out)
  }
}
