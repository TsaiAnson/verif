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

import com.verif._

class ProtoTest extends FlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = VerifTestUtils.getVerifParameters()

  it should "Protobuf Test" in {
    test(new Queue(UInt(8.W), 8)).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      val cmd = RoCCProtos.RoCCCommand.newBuilder()
        .setRs2(((BigInt(2) << 48) + (BigInt(1) << 32)).toLong)
        .setInst(
          RoCCProtos.RoCCInstruction.newBuilder()
            .setFunct(2))
        .build()

      val bundle = VerifProtoBufUtils.ProtoToBundle(cmd, VerifRoCCUtils, new RoCCCommand)

      println("--- PROTO MESSAGE ---")
      println(cmd)
      println("--- CHISEL BUNDLE ---")
      println(bundle)

      assert(true)
    }
  }
}