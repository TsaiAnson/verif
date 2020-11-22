package verif

import org.scalatest._
import designs._
import chisel3._
import chiseltest._
import chisel3.util._
import chisel3.stage.ChiselGeneratorAnnotation
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation, TreadleBackendAnnotation, WriteVcdAnnotation}
import testchipip.{TLHelper}
import freechips.rocketchip.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.system._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import firrtl.AnnotationSeq
import gemmini._

import com.verif._

// NOTE: Need to run "sbt -mem 4096" to have enough heap space (4GB) to easily elaborate Gemmini
class GemminiTest extends FlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = VerifTestUtils.getVerifParameters()

  val dut = LazyModule(
    new VerifRoCCStandaloneWrapper(
      () => new Gemmini(OpcodeSet.custom3, GemminiConfigs.defaultConfig),
      beatBytes=16
    ))
  it should "Elaborate Gemmini" in {
    test(dut.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      // Drivers
      val commandDriver = new DecoupledDriver[RoCCCommand](c.clock, dut.module.io.cmd)
      val ptwRespDriver = new ValidDriver[PTWResp](c.clock, dut.module.io.ptw(0).resp)
      // TODO: tlClientDriver is broken
      //val tlDriver = new TLClientDriverBasic(c.clock, dut.module.tlOut)

      // Monitors
      val ptwReqMonitor = new DecoupledMonitor[ValidIO[PTWReq]](c.clock, dut.module.io.ptw(0).req)
      val tlMonitor = new TLClientMonitorBasic(c.clock, dut.module.tlOut)

      // MVIN 2 ROW and 1 COL
      commandDriver.push(DecoupledTX(
        VerifBundleUtils.RoCCCommandHelper(
        inst = VerifBundleUtils.RoCCInstructionHelper(funct = 2.U),
        rs2 = fromBigIntToLiteral((BigInt(2) << 48) + (BigInt(1) << 32)).asUInt
      )))

      c.clock.step(500)
      assert(true)
      // open socket
      // send ack over socket
      // assert false if doesn't exits
    }
  }
}