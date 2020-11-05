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

  val smallGemminiConfig = GemminiArrayConfig[SInt, SInt](
    // val defaultConfig = GemminiArrayConfig[Float, Float](
    tileRows = 1,
    tileColumns = 1,
    // meshRows = 4,
    // meshColumns = 4,
    meshRows = 4,
    meshColumns = 4,
    ld_queue_length = 8,
    st_queue_length = 2,
    ex_queue_length = 8,
    rob_entries = 16,
    sp_banks = 4,
    acc_banks = 1,
    sp_capacity = CapacityInKilobytes(256),
    shifter_banks = 1, // TODO add separate parameters for left and up shifter banks
    dataflow = Dataflow.BOTH,
    acc_capacity = CapacityInKilobytes(64),
    mem_pipeline = 1,
    dma_maxbytes = 64, // TODO get this from cacheblockbytes
    dma_buswidth = 128, // TODO get this from SystemBusKey
    aligned_to = 1,
    inputType = SInt(8.W),
    outputType = SInt(19.W),
    accType = SInt(32.W),
    // inputType = Float(8, 24),
    // outputType = Float(8, 24),
    // accType = Float(8, 24),
    // mvin_scale_args = Some(MvinScaleArguments((t: SInt, u: SInt) => t * u, 0, SInt(8.W))),
    // mvin_scale_acc_args = Some(MvinScaleArguments((t: SInt, u: SInt) => t * u, 0, SInt(8.W))),
    mvin_scale_args = None,
    mvin_scale_acc_args = None,
    // mvin_scale_args = Some(MvinScaleArguments((t: Float, u: Float) => t * u, 0, Float(8, 24))),
    // mvin_scale_acc_args = Some(MvinScaleArguments((t: Float, u: Float) => t * u, 0, Float(8, 24))),
    mvin_scale_shared = false,
    pe_latency = 0
  )

  val dut = LazyModule(
    new VerifRoCCStandaloneWrapper(
      () => new Gemmini(OpcodeSet.custom3, smallGemminiConfig),
      beatBytes=16
    ))
  it should "Elaborate Gemmini" in {
    test(dut.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      assert(true)
    }
  }
}