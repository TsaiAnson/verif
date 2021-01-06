package verif

import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest._
import designs._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule}
import freechips.rocketchip.subsystem.WithoutTLMonitors
import verifTLUtils._

import scala.collection.mutable.HashMap

class TLL2CacheTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = new WithoutTLMonitors

  it should "Elaborate L2" in {
    val TLL2 = LazyModule(new VerifTLL2Cache)
    test(TLL2.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      val L1Placeholder = new TLDriverMaster(c.clock, TLL2.in)
      val monitor = new TLMonitor(c.clock, TLL2.in, hasBCE = true)
      val monitor1 = new TLMonitor(c.clock, TLL2.out, hasBCE = false)

      val test = HashMap[Int,Int]()
      test(0) = 0x1234
      test(8) = 0x3333
      val DRAMPlaceholder = new TLDriverSlave(c.clock, TLL2.out, test, testResponse)

      L1Placeholder.push(Seq(Get(size = 3.U, source = 0.U, addr = 0.U, mask = 0xff.U)))
      L1Placeholder.push(Seq(AcquireBlock(param = 1.U, size = 3.U, source = 0.U, addr = 0x8.U, mask = 0xff.U)))

      c.clock.step(200)

      println("INNER (CORE)")
      for (t <- monitor.getMonitoredTransactions()) {
        println(t)
      }
      println("OUTER (DRAM)")
      for (t <- monitor1.getMonitoredTransactions()) {
        println(t)
      }
    }
  }

  it should "Test New Driver Master" in {
    val TLL2 = LazyModule(new VerifTLL2Cache)
    test(TLL2.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      val L1Placeholder = new TLDriverMasterNew(c.clock, TLL2.in)
      val monitor = new TLMonitor(c.clock, TLL2.in, hasBCE = true)
      val monitor1 = new TLMonitor(c.clock, TLL2.out, hasBCE = false)

      val test = HashMap[Int,Int]()
      val DRAMPlaceholder = new TLDriverSlave(c.clock, TLL2.out, test, testResponse)

      val txns = Seq(
        // Two Acquires in a row, must be sequential
        AcquireBlock(param = 1.U, size = 3.U, source = 0.U, addr = 0x0.U, mask = 0xff.U),
        AcquireBlock(param = 0.U, size = 3.U, source = 0.U, addr = 0x20.U, mask = 0xff.U),
        // Cannot acquire until release completes
        ReleaseData(param = 0.U, size = 3.U, source = 0.U, addr = 0x20.U, data = 0.U(64.W)),
        AcquireBlock(param = 1.U, size = 3.U, source = 0.U, addr = 0x40.U, mask = 0xff.U),
        // L2 with sets = 2 will evict a block after third Acquire
      )

      L1Placeholder.push(txns)
      c.clock.step(200)

      println("PERM STATE")
      val perm = L1Placeholder.permState
      for (x <- perm.keys) {
        print(s"(${x}, ${perm(x)}), ")
      }
      println("")

      println("INNER (CORE)")
      for (t <- monitor.getMonitoredTransactions()) {
        println(t)
      }
      println("OUTER (DRAM)")
      for (t <- monitor1.getMonitoredTransactions()) {
        println(t)
      }
    }
  }

  it should "L2 SWTLFuzzer" in {

    val TLL2 = LazyModule(new VerifTLL2Cache)
    test(TLL2.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      val L1Placeholder = new TLDriverMasterNew(c.clock, TLL2.in, allowInvalidTxn = false, fixInvalidTxn = true)
      val monitor = new TLMonitor(c.clock, TLL2.in, hasBCE = true)
      val monitor1 = new TLMonitor(c.clock, TLL2.out, hasBCE = false)

      val test = HashMap[Int,Int]()
      val DRAMPlaceholder = new TLDriverSlave(c.clock, TLL2.out, test, testResponse)

      val fuz = new SWTLFuzzer(standaloneSlaveParams.managers(0), overrideAddr = Some(AddressSet(0x00, 0x1ff)),
        get = false, putPartial = false, putFull = false,
        burst = true, arith = false, logic = false, hints = false, acquire = true, tlc = true)
      val txns = fuz.generateTransactions(30)

      println("TXNS")
      for (t <- txns) {
        println(t)
      }

      L1Placeholder.push(txns)
      c.clock.step(500)

      println("PERM STATE")
      val perm = L1Placeholder.permState
      for (x <- perm.keys) {
        print(s"(${x}, ${perm(x)}), ")
      }
      println("")

      println("INNER (CORE)")
      for (t <- monitor.getMonitoredTransactions()) {
        println(t)
      }
      println("OUTER (DRAM)")
      for (t <- monitor1.getMonitoredTransactions()) {
        println(t)
      }
    }
  }
}