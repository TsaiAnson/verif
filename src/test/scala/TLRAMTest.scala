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

//  it should "VerifTL Test TLRAM via SWTLFuzzer" in {
//    val TLRAMSlave = LazyModule(new VerifTLRAMSlave with VerifTLStandaloneBlock)
//    test(TLRAMSlave.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
//
//      val passInAgent = new TLDriverMaster(c.clock, TLRAMSlave.in)
//      val passOutAgent = new TLMonitorMaster(c.clock, TLRAMSlave.in)
//      val simCycles = 150
//
//      val fuz = new SWTLFuzzer(TLRAMSlave.slaveParams.managers(0), overrideAddr = Some(AddressSet(0x00, 0x1ff)))
//      val inputTransactions = fuz.generateTransactions(60)
//
//      passInAgent.push(inputTransactions)
//      c.clock.step(simCycles)
//
//      val output = passOutAgent.getMonitoredTransactions.toArray
//
//      for (out <- output) {
//        println(out)
//      }
////      assert(outputChecker.checkOutput(output, {t : TLTransaction => t},
////        swoutput, {t : TLTransaction => t}))
//    }
//  }

//  it should "Driver/Monitor Master Hardcoded Burst TLRAM" in {
//    val TLRAMSlave = LazyModule(new VerifTLRAMSlave with VerifTLStandaloneBlock)
//    test(TLRAMSlave.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
//
//      val passInAgent = new TLDriverMaster(c.clock, TLRAMSlave.in)
//      val passOutAgent = new TLMonitorMaster(c.clock, TLRAMSlave.in)
//      val simCycles = 150
//
//      val inputTransactions = Seq(
//        PutFullBurst(size = 4.U, addr = 0x10.U, masks = List(0xff.U, 0xff.U), datas = List(0x1234.U, 0x5678.U)),
//        Get(size = 4.U, addr = 0x10.U, mask = 0xff.U)
//      )
//
//      passInAgent.push(inputTransactions)
//      c.clock.step(simCycles)
//
//      val output = passOutAgent.getMonitoredTransactions.toArray
//
//      for (out <- output) {
//        println(out)
//      }
//    }
//  }

  it should "Basic Unittest of UH Transactions (Atomics, Hints)" in {
    val TLRAMSlave = LazyModule(new VerifTLRAMSlave with VerifTLStandaloneBlock)
    test(TLRAMSlave.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      val passInAgent = new TLDriverMaster(c.clock, TLRAMSlave.in)
      val passOutAgent = new TLMonitorMaster(c.clock, TLRAMSlave.in)
      val simCycles = 150

      // Note that there are no bursts - Will add once I figure out issue
      // Note that there are no hints - Some assertions fail in Model when used.
      val inputTransactions = Seq(
//        Intent(param = 1.U, size = 3.U, addr = 0x0.U, mask = 0xff.U), PutFull(addr = 0x0.U, mask = 0xff.U, data = 0x1234.U),
//        Intent(param = 1.U, size = 3.U, addr = 0x0.U, mask = 0xff.U), Get(size = 3.U, addr = 0x0.U, mask = 0xff.U),
        PutFull(addr = 0x0.U, mask = 0xff.U, data = 0x1234.U),
        Get(size = 3.U, addr = 0x0.U, mask = 0xff.U),
        ArithData(param = 4.U, addr = 0x0.U, mask = 0xff.U, data = 0x1.U), Get(size = 3.U, addr = 0x0.U, mask = 0xff.U),
        LogicData(param = 2.U, addr = 0x0.U, mask = 0xff.U, data = 0xfff0.U), Get(size = 3.U, addr = 0x0.U, mask = 0xff.U)
      )

      passInAgent.push(inputTransactions)
      c.clock.step(simCycles)

      val output = passOutAgent.getMonitoredTransactions.toArray

      for (out <- output) {
        println(out)
      }
    }
  }
}
