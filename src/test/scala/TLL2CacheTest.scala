package verif

import org.scalatest._
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

class TLL2CacheTest extends FlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = new WithoutTLMonitors

  it should "Elaborate L2" in {
    val TLL2 = LazyModule(new VerifTLL2Cache)
    test(TLL2.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>

      val L1Placeholder = new TLDriverMaster(c.clock, TLL2.in)
      val monitor = new TLMonitor(c.clock, TLL2.in)
//      val ctrlPlaceholder = new TLDriverMaster(c.clock, TLL2.ctrl)

      val DRAMPlaceholder = new TLDriverSlave(c.clock, TLL2.out, HashMap[Int,Int](), testResponse)

      val simCycles = 150

      c.clock.step(simCycles)
    }
  }
}