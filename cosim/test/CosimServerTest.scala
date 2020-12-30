package verif

import org.scalatest._

import designs._
import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation, TreadleBackendAnnotation, WriteVcdAnnotation}
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.diplomacy.{LazyModule}
import freechips.rocketchip.tile.{OpcodeSet, RoCCCommand, RoCCIO}
import gemmini._

import cosim._
import com.verif._


/*class CosimServerTest extends FlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = VerifTestUtils.getVerifParameters()

  val dut = LazyModule(
    new VerifRoCCStandaloneWrapper(
      () => new Gemmini(OpcodeSet.custom3, GemminiConfigs.defaultConfig.copy(use_dedicated_tl_port = true, meshRows = 4,
        meshColumns = 4, rob_entries = 4)),
      beatBytes = 16,
      addSinks = 1
    ))

  it should "Cosim Driver Server Test" in {
    test(dut.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      // Setup command driver
      val cmdDriver = new DecoupledDriver[RoCCCommand](c.clock, c.io.cmd)

      // Set up driver server and client
      val port = 8080
      val target = s"localhost:$port"

      val channel = io.grpc.ManagedChannelBuilder.forTarget(target)
        .usePlaintext()
        .build();
      val server = new RoCCCommandCosimDriverServer(cmdDriver,
        (cmd: com.google.protobuf.Message) => VerifProtoBufUtils.ProtoToBundle(cmd, VerifRoCCUtils, new RoCCCommand), port = 8080)
      val client = new RoCCCommandCosimClient(channel)

      val cmd = RoCCProtos.RoCCCommand.newBuilder()
        .setRs2(((BigInt(2) << 48) + (BigInt(1) << 32)).toLong)
        .setInst(
          RoCCProtos.RoCCInstruction.newBuilder()
            .setFunct(2))
        .build()

      val thread = new Thread {
        override def run: Unit = {
          server.main()
        }

        def terminateServer: Unit = {
          server.stop()
        }
      }

      // Do everything in a try catch to terminate server if necessary on failure
      try {
        thread.start

        client.send(cmd)

        c.clock.step(100)


      } catch {
        case _: Throwable => println("The test threw and exception, will attempt cleanup")
      }

      // Cleanup *IMPORTANT*
      thread.terminateServer
      thread.stop
      channel.shutdownNow().awaitTermination(5, java.util.concurrent.TimeUnit.SECONDS);

      // Provide time for threads to terminate *IMPORTANT*
      Thread.sleep(30*1000)

      assert(true)
    }
  }
}*/