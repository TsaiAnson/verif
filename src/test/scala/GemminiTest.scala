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
      val passInAgent = new RoCCDriver(c.clock, dut.module.io.cmdIn)
      val passOutAgent = new TLClientMonitorBasic(c.clock, dut.module.tlOut)

      // MVIN 1 ROW and 1 COL
      passInAgent.push(RoCCVerifUtils.RoCCCommandHelper(
        inst = RoCCVerifUtils.RoCCInstructionHelper(funct = 2.U),
        rs2 = fromBigIntToLiteral((BigInt(1) << 48) + (BigInt(1) << 32)).asUInt
      ))
      c.clock.step(500)
      assert(true)
      // open socket
      // send ack over socket
      // assert false if doesn't exits
    }
  }
}