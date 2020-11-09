package verif

import TestUtils.SWRegBank
import designs._
import org.scalatest._
import chisel3._
import chiseltest._
import designs._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule}
import freechips.rocketchip.subsystem.WithoutTLMonitors

class SWTLFuzzerTest extends FlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = new WithoutTLMonitors

  it should "VerifTL Test RegBank via SWTLFuzzer" in {
    val TLRegBankSlave = LazyModule(new VerifTLRegBankSlave with VerifTLStandaloneBlock)
    test(TLRegBankSlave.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      val passInAgent = new TLSlaveDriverBasic(c.clock, TLRegBankSlave.in)
      val passOutAgent = new TLSlaveMonitorBasic(c.clock, TLRegBankSlave.in)
      val simCycles = 150

      val fuz = new SWTLFuzzer(TLRegBankSlave.standaloneSlaveParams.managers(0), overrideAddr = Some(AddressSet(0x00, 0x1ff)))
      val inputTransactions = fuz.generateTransactions(60)

      passInAgent.push(inputTransactions)
      c.clock.step(simCycles)

      val output = passOutAgent.getMonitoredTransactions.toArray

      val model = new SWRegBank(regCount = 64, regSizeBytes = 8)
      val swoutput = model.process(inputTransactions).toArray

      assert(outputChecker.checkOutput(output, {t : TLTransaction => t},
        swoutput, {t : TLTransaction => t}))
    }
  }
}
