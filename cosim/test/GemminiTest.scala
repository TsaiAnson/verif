package verif

import chisel3._
import chiseltest._
import chisel3.util._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation, TreadleBackendAnnotation, WriteVcdAnnotation}
import cosim._
import designs._
import firrtl.AnnotationSeq
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.diplomacy.{LazyModule}
import freechips.rocketchip.tile.{RoCCCommand, OpcodeSet}
import freechips.rocketchip.rocket.{PTWResp, PTWReq}
import gemmini._
import org.scalatest._

import com.verif._

// NOTE: Need to run "sbt -mem 4096" to have enough heap space (4GB) to easily elaborate Gemmini
class GemminiTest extends FlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = VerifTestUtils.getVerifParameters()

  val dut = LazyModule(
    new VerifRoCCStandaloneWrapper(
      () => new Gemmini(OpcodeSet.custom3, GemminiConfigs.defaultConfig.copy(use_dedicated_tl_port = true, meshRows = 4,
                        meshColumns = 4, rob_entries = 4)),
      beatBytes = 16,
      addSinks = 1
    ))
  it should "Elaborate Gemmini" in {
    test(dut.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      // Drivers
      val commandDriver = new DecoupledDriver[RoCCCommand](c.clock, c.io.cmd)
      val ptwRespDriver = new ValidDriver[PTWResp](c.clock, c.io.ptw(0).resp)
      // TODO: tlClientDriver is broken
      //val tlDriver = new TLClientDriverBasic(c.clock, dut.module.tlOut)

      // Monitors
      val ptwReqMonitor = new DecoupledMonitor[ValidIO[PTWReq]](c.clock, c.io.ptw(0).req)
      val tlMonitor = new TLClientMonitorBasic(c.clock, c.tlOut(0))

      // Cosim servers
//      val commandServer = new CosimDriverServer(commandDriver,
//        com.verif.RoCCProtos.RoCCCommand.parseFrom,
//        (cmd: com.google.protobuf.Message) => VerifProtoBufUtils.ProtoToBundle(cmd, VerifRoCCUtils, new RoCCCommand))
//
//      // MVIN, 2 ROW and 1 COL
//      val cmd = RoCCProtos.RoCCCommand.newBuilder()
//        .setRs2(((BigInt(2) << 48) + (BigInt(1) << 32)).toLong)
//        .setInst(
//          RoCCProtos.RoCCInstruction.newBuilder()
//            .setFunct(2))
//        .build()
//
//      commandServer.start
//
//      CosimClient.sendProto(cmd)
//
//      commandServer.terminate
//
      c.clock.step(500)
      assert(true)
    }
  }
}