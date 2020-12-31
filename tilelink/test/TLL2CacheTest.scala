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
    test(TLL2.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>

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
}