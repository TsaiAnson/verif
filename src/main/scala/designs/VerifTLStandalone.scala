package designs

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink.TLRegisterNode
import verif.VerifTLUtils._

trait VerifTLStandaloneBlock extends LazyModule {
  val masterParams = standaloneMasterParams
  val slaveParams = standaloneSlaveParams

  val ioInNode = BundleBridgeSource(() => TLBundle(verifTLBundleParams))
  val ioOutNode = BundleBridgeSink[TLBundle]()

  val TLMaster: TLOutwardNode
  val TLSlave: TLInwardNode

   ioOutNode :=
     TLToBundleBridge(standaloneSlaveParams) := TLMaster

  TLSlave :=
    BundleBridgeToTL(standaloneMasterParams) :=
    ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }
}

class VerifTLRegBankSlave(implicit p: Parameters) extends LazyModule  {
  val device = new SimpleDevice("VerifTLRegBankSlave", Seq("veriftldriver,veriftlmonitor,testmaster")) // Not sure about compatibility list

  val TLSlave = TLRegisterNode(
    address = Seq(AddressSet(0x0, 0xfff)),
    device = device,
    beatBytes = 8,
    concurrency = 1)

  // Filler for now
  val TLMaster = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
    name = "testmaster",
    sourceId = IdRange(0,1),
    requestFifo = true,
    visibility = Seq(AddressSet(0x1000, 0xfff)))))))

  lazy val module = new LazyModuleImp(this) {
    val regs = RegInit(VecInit(Seq.fill(64)(0.U(64.W))))

    val tuples = regs.zipWithIndex.map { case (reg, i) =>
      (0x00 + (i * 8)) -> Seq(RegField(64,reg))
    }
    TLSlave.regmap(tuples :_*)
  }
}

class VerifTLRAMSlave(implicit p: Parameters) extends LazyModule {
  val device = new SimpleDevice("VerifTLRAMSlave", Seq("veriftldriver,veriftlmonitor,testmaster"))

  val model = LazyModule(new TLRAMModel("TLFuzzRAM"))
  val ram  = LazyModule(new TLRAM(AddressSet(0x0, 0x1ff), beatBytes = 8))
  ram.node := model.node
  val TLSlave = model.node

  // Filler for now
  val TLMaster = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
    name = "testmaster",
    sourceId = IdRange(0,1),
    requestFifo = true,
    visibility = Seq(AddressSet(0x1000, 0xfff)))))))

  lazy val module = new LazyModuleImp(this) {}
}

// Example of manual Master
class VerifTLCustomMaster(implicit p: Parameters) extends LazyModule  {
  val device = new SimpleDevice("VerifTLCustomMaster", Seq("veriftldriver,veriftlmonitor,testmaster"))

  // Filler for now
  val TLSlave = TLRegisterNode(
    address = Seq(AddressSet(0x0, 0xfff)),
    device = device,
    beatBytes = 8,
    concurrency = 1)

    val TLMaster = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
      name = "testmaster",
      sourceId = IdRange(0,1),
      requestFifo = true,
      visibility = Seq(AddressSet(0x0, 0xfff)))))))

  lazy val module = new LazyModuleImp(this) {
    val (out, edge) = TLMaster.out(0)
    val addr = RegInit(UInt(8.W), 0.U)
    val response = RegInit(UInt(5.W), 0.U)
    val alt = RegInit(Bool(), false.B)

    // Behavior: Read in address x and then write result in address + 0x20.U
    // Operates on addresses from 0x0 to 0x18

    // Offset by 8 needed here, will look into later
    when (alt) {
      out.a.bits := edge.Get(0.U, addr - 0x8.U, 3.U)._2
    } otherwise {
      out.a.bits := edge.Put(0.U, addr + 0x18.U, 3.U, response)._2
    }

    when (out.a.fire()) {
      when (!alt) {
        addr := addr + 0x8.U
      }
      alt := !alt
    }

    when (out.d.valid) {
      response := out.d.deq().data
    }

    // Hack to fix missing last instruction, fix later
    out.a.valid := addr < 0x20.U || (addr === 0x20.U && alt)
  }
}

class VerifTLMasterPattern(txns: Seq[Pattern])(implicit p: Parameters) extends LazyModule  {
  val device = new SimpleDevice("VerifTLMasterPattern", Seq("veriftldriver,veriftlmonitor,testmaster"))

  // Filler for now
  val TLSlave = TLRegisterNode(
    address = Seq(AddressSet(0x0, 0xfff)),
    device = device,
    beatBytes = 8,
    concurrency = 1)

  val patternp = LazyModule(new TLPatternPusher("patternpusher", txns))
  val TLMaster = patternp.node

  lazy val module = new LazyModuleImp(this) {
    val testIO = IO(new Bundle {
      val run = Input(Bool())
      val done = Output(Bool())
    })

    RegNext(patternp.module.io.run) := testIO.run
    testIO.done := patternp.module.io.done
  }
}

class VerifTLMasterFuzzer(implicit p: Parameters) extends LazyModule  {
  val device = new SimpleDevice("VerifTLMasterFuzzer", Seq("veriftldriver,veriftlmonitor,testmaster"))

  // Filler for now
  val TLSlave = TLRegisterNode(
    address = Seq(AddressSet(0x0, 0xfff)),
    device = device,
    beatBytes = 8,
    concurrency = 1)

  val TLMaster = TLFuzzer(30, inFlight=1)

  lazy val module = new LazyModuleImp(this) {}
}
