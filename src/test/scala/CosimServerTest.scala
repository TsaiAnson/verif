package verif

import org.scalatest._

import designs._
import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation, TreadleBackendAnnotation, WriteVcdAnnotation}
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.tile.{RoCCCommand}

import cosim._
import com.verif._


class CosimServerTest extends FlatSpec with ChiselScalatestTester {
  it should "Cosim Server Test" in {
    test(new Queue(UInt(8.W), 8)).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      val server = new CosimServer()

      server.start

      CosimClient.main(str = "First Message")
      CosimClient.main(str = "Second Message")

      server.terminate

      assert(true)
    }
  }
}