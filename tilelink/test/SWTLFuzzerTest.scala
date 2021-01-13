package verif

import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule}
import freechips.rocketchip.subsystem.WithoutTLMonitors

class SWTLFuzzerTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = new WithoutTLMonitors

  // Ignoring test since SWRegBank is outdated
  it should "VerifTL Test RegBank via SWTLFuzzer" ignore {
    val TLRegBankSlave = LazyModule(new VerifTLRegBankSlave)
    test(TLRegBankSlave.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

//      val passInAgent = new TLDriverMaster(c.clock, TLRegBankSlave.in)
//      val passOutAgent = new TLMonitor(c.clock, TLRegBankSlave.in)
//      val simCycles = 150
//
//      val fuz = new SWTLFuzzer(standaloneSlaveParams.managers(0), overrideAddr = Some(AddressSet(0x00, 0x1ff)))
//      val inputTransactions = fuz.generateTransactions(60)
//
//      passInAgent.push(inputTransactions)
//      c.clock.step(simCycles)
//
//      val output = passOutAgent.getMonitoredTransactions(filterD).toArray
//
//      val model = new SWRegBank(regCount = 64, regSizeBytes = 8)
//      val swoutput = model.process(inputTransactions).toArray
//
//      assert(outputChecker.checkOutput(output, {t : TLTransaction => t},
//        swoutput, {t : TLTransaction => t}))
    }
  }
}
