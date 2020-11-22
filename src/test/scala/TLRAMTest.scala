package verif

import org.scalatest._
import chisel3._
import chiseltest._
import designs._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule}
import freechips.rocketchip.subsystem.WithoutTLMonitors

class TLRAMTest extends FlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = new WithoutTLMonitors

  it should "VerifTL Test TLRAM via SWTLFuzzer" in {
    val TLRAMSlave = LazyModule(new VerifTLRAMSlave with VerifTLStandaloneBlock)
    test(TLRAMSlave.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      val passInAgent = new TLDriverMaster(c.clock, TLRAMSlave.in)
      val passOutAgent = new TLMonitorMaster(c.clock, TLRAMSlave.in)
      val simCycles = 150

      val fuz = new SWTLFuzzer(TLRAMSlave.slaveParams.managers(0), overrideAddr = Some(AddressSet(0x00, 0x1ff)))
      val inputTransactions = fuz.generateTransactions(60)

      passInAgent.push(inputTransactions)
      c.clock.step(simCycles)

      val output = passOutAgent.getMonitoredTransactions.toArray

      for (out <- output) {
        println(out)
      }
//      assert(outputChecker.checkOutput(output, {t : TLTransaction => t},
//        swoutput, {t : TLTransaction => t}))
    }
  }
}
