package verif

import org.scalatest._

import designs._
import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation, TreadleBackendAnnotation, WriteVcdAnnotation}
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.tile.{RoCCCommand, RoCCIO}

import cosim._
import com.verif._


class CosimServerTest extends FlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = VerifTestUtils.getVerifParameters()

  it should "Cosim Driver Server Test" in {
    test(new Queue(UInt(8.W), 8)).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      val io = new RoCCIO(1)
      val cmdDriver = new DecoupledDriver[RoCCCommand](c.clock, io.cmd)
      val server = new CosimDriverServer(cmdDriver,
        com.verif.RoCCProtos.RoCCCommand.parseFrom,
        (cmd: com.google.protobuf.Message) => VerifProtoBufUtils.ProtoToBundle(cmd, VerifRoCCUtils, VerifRoCCUtils, new RoCCCommand))

      val cmd = RoCCProtos.RoCCCommand.newBuilder()
        .setRs2(((BigInt(2) << 48) + (BigInt(1) << 32)).toLong)
        .setInst(
          RoCCProtos.RoCCInstruction.newBuilder()
            .setFunct(2))
        .build()

      server.start

      CosimClient.sendProto(cmd)

      server.terminate

      assert(true)
    }
  }
}