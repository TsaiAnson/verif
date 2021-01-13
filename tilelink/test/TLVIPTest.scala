package verif

import chipsalliance.rocketchip.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.WithoutTLMonitors
import chiseltest.experimental.TestOptionBuilder._
import verif.TLUtils._
import TLTransaction._
import freechips.rocketchip.tilelink.TLBundleA

class TLVIPTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = new WithoutTLMonitors

  it should "Temp Sanity Test for New SDriver Skeleton" in {
    val TLCustomMaster = LazyModule(new VerifTLCustomMaster)
    test(TLCustomMaster.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      implicit val params = TLCustomMaster.out.params

      // Currently just recording requests, only Driver is needed
      val sDriver = new TLDriverSlave(c.clock, TLCustomMaster.out, SlaveMemoryState.init(), testResponseWrapper)
      val monitor = new TLMonitor(c.clock, TLCustomMaster.out)
      val simCycles = 80

      // State testing
      val init_state = SlaveMemoryState.init()
      sDriver.state = init_state

      c.clock.step(simCycles)

      val output = monitor.getMonitoredTransactions().toArray

      // Transactions
      for (out <- output) {
        println(out)
      }

      // State Map
      println("Resulting State")
      val hash = sDriver.state
      for (x <- hash.mem.keys) {
        print(s"(${x}, ${hash.mem(x)}), ")
      }
      println("")

      // Init State (making sure that the original state was not modified)
      println("Initial State")
      for (x <- init_state.mem.keys) {
        print(s"(${x}, ${init_state.mem(x)}), ")
      }
      println("")
    }
  }

  it should "Test for New SDriver" in {
    val TLFeedback = LazyModule(new VerifTLMasterSlaveFeedback)
    test(TLFeedback.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      implicit val params = TLFeedback.in.params

      // Drivers/Monitors
      val mDriver = new TLDriverMaster(c.clock, TLFeedback.in)
      val sDriver = new TLDriverSlave(c.clock, TLFeedback.out, SlaveMemoryState.init(), testResponseWrapper)

      val monitor = new TLMonitor(c.clock, TLFeedback.in)

      val simCycles = 500

      val inputTransactions: Seq[TLBundleA] = Seq(
        Get(0x0),
        Put(0x0, 0x3333),
        Get(0x0)
      ) ++
        (PutBurst(0x0, Seq(0x3333, 0x1234), 0) :+
        Get(0x0) :+
        Get(0x8) :+
        Logic(2, 0x0, 0x0)) ++
        (LogicBurst(2, 0x0, Seq(0x0, 0x0)) :+
        Get(0x0) :+
        Get(0x8) :+
        Arith(4, 0x0, 0x0) :+
        Get(0x0)) ++
        ArithBurst(4, 0x0, Seq(0x0, 0x0)) :+
        Get(0x0) :+
        Get(0x8)

      mDriver.push(inputTransactions)
      c.clock.step(simCycles)

      val output = monitor.getMonitoredTransactions().filter(filterD).toArray

      // Transactions
      for (out <- output) {
        println(out)
      }

      // State Map
      val hash = sDriver.state
      for (x <- hash.mem.keys) {
        print(s"(${x}, ${hash.mem(x)}), ")
      }
      println("")
    }
  }
}