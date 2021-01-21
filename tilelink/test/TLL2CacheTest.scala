package verif

import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule}
import freechips.rocketchip.subsystem.WithoutTLMonitors
import verif.TLUtils._
import TLTransaction._
import freechips.rocketchip.tilelink._

class TLL2CacheTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = new WithoutTLMonitors

  it should "Elaborate L2" in {
    val TLL2 = LazyModule(new VerifTLL2Cache)
    test(TLL2.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>

      val L1Placeholder = new TLDriverMaster(c.clock, TLL2.in)
      val L1Monitor = new TLMonitor(c.clock, TLL2.in)
      val DRAMMonitor = new TLMonitor(c.clock, TLL2.out)

      val slaveFn = new TLMemoryModel(TLL2.out.params)
      val DRAMPlaceholder = new TLDriverSlave(c.clock, TLL2.out, slaveFn, TLMemoryModel.State.init(Map(0L -> 0x1234, 1L -> 0x3333), TLL2.out.params.dataBits/8))

      L1Placeholder.push(Seq(AcquireBlock(param = 1, addr = 0x8, size = 5)(TLL2.in.params)))

      c.clock.step(200)

      val output1 = L1Monitor.getMonitoredTransactions().map(_.data).collect{ case t: TLBundleD => t}
      val sanity1 = new TLSanityChecker(TLL2.in.params, standaloneSlaveParamsC.managers.head, standaloneMasterParamsC.clients.head)
      sanity1.sanityCheck(output1)

      val output2 = DRAMMonitor.getMonitoredTransactions().map(_.data).collect{ case t: TLBundleD => t}
      val sanity2 = new TLSanityChecker(TLBundleParameters(standaloneMasterParams, standaloneSlaveParams),
        standaloneSlaveParams.managers.head, standaloneMasterParams.clients.head)
      sanity2.sanityCheck(output2)

//      println("INNER (CORE)")
//      for (t <- monitor.getMonitoredTransactions()) {
//        println(t)
//      }
//      println("OUTER (DRAM)")
//      for (t <- monitor1.getMonitoredTransactions()) {
//        println(t)
//      }
    }
  }

  // Ignoring test as new driver is no longer TLC compliance
  it should "Driver TLC Compliance Test" in {
    val TLL2 = LazyModule(new VerifTLL2Cache)
    test(TLL2.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      val L1PortParams = TLL2.in.params
      val DRAMPortParams = TLL2.out.params

      val L1Placeholder = new TLDriverMaster(c.clock, TLL2.in)
      val FuzzMonitor = new TLMonitor(c.clock, TLL2.in)
      val L1Monitor = new TLMonitor(c.clock, TLL2.in)
      val DRAMMonitor = new TLMonitor(c.clock, TLL2.out)

      val slaveFn = new TLMemoryModel(DRAMPortParams)
      val DRAMPlaceholder = new TLDriverSlave(c.clock, TLL2.out, slaveFn, TLMemoryModel.State.empty())

      val txns = Seq(
        // Two Acquires in a row, must be sequential
        AcquireBlock(param = 1, addr = 0x0, size = 3)(L1PortParams),
        AcquireBlock(param = 0, addr = 0x20, size = 3)(L1PortParams),
        // Cannot acquire until release completes
        ReleaseData(param = 0, addr = 0x20, data = 0x0, size = 3, source = 0)(L1PortParams),
        AcquireBlock(param = 1, addr = 0x40, size = 3)(L1PortParams),
        // L2 with sets = 2 will evict a block after third Acquire
      )

      val gen = new TLTransactionGenerator(standaloneSlaveParamsC.managers(0), TLL2.in.params, overrideAddr = Some(AddressSet(0x00, 0x1ff)),
        get = false, putPartial = false, putFull = false,
        burst = true, arith = false, logic = false, hints = false, acquire = true, tlc = true, cacheBlockSize = 3)
      val fuzz = new TLCFuzzer(L1PortParams, gen, 3, txns, true)

      for (_<- 0 until 20) {
        val txns = fuzz.fuzzTxn(FuzzMonitor.getMonitoredTransactions().map({_.data}))
        L1Placeholder.push(txns)
        c.clock.step(5)
      }

      val output1 = L1Monitor.getMonitoredTransactions().map(_.data).collect{ case t: TLBundleD => t}
      val sanity1 = new TLSanityChecker(TLL2.in.params, standaloneSlaveParamsC.managers.head, standaloneMasterParamsC.clients.head)
      sanity1.sanityCheck(output1)

      val output2 = DRAMMonitor.getMonitoredTransactions().map(_.data).collect{ case t: TLBundleD => t}
      val sanity2 = new TLSanityChecker(TLBundleParameters(standaloneMasterParams, standaloneSlaveParams),
        standaloneSlaveParams.managers.head, standaloneMasterParams.clients.head)
      sanity2.sanityCheck(output2)

//      println("INNER (CORE)")
//      for (t <- L1Monitor.getMonitoredTransactions()) {
//        println(t)
//      }
//      println("OUTER (DRAM)")
//      for (t <- DRAMMonitor.getMonitoredTransactions()) {
//        println(t)
//      }
    }
  }

  it should "L2 SWTLFuzzer" in {

    val TLL2 = LazyModule(new VerifTLL2Cache)
    test(TLL2.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      implicit val params = TLL2.in.params

      val L1Placeholder = new TLDriverMaster(c.clock, TLL2.in)
      val FuzzMonitor = new TLMonitor(c.clock, TLL2.in)
      val L1Monitor = new TLMonitor(c.clock, TLL2.in)
      val DRAMMonitor = new TLMonitor(c.clock, TLL2.out)

      val slaveFn = new TLMemoryModel(params)
      val DRAMPlaceholder = new TLDriverSlave(c.clock, TLL2.out, slaveFn, TLMemoryModel.State.empty())

      val gen = new TLTransactionGenerator(standaloneSlaveParamsC.managers(0), TLL2.in.params, overrideAddr = Some(AddressSet(0x00, 0x1ff)),
        get = false, putPartial = false, putFull = false,
        burst = true, arith = false, logic = false, hints = false, acquire = true, tlc = true, cacheBlockSize = 5)
      val fuzz = new TLCFuzzer(params, gen, 5)

      for (i <- 0 until 200) {
        val txns = fuzz.fuzzTxn(FuzzMonitor.getMonitoredTransactions().map({_.data}))
        L1Placeholder.push(txns)
        c.clock.step(5)
      }

      val output1 = L1Monitor.getMonitoredTransactions().map(_.data).collect{ case t: TLBundleD => t}
      val sanity1 = new TLSanityChecker(TLL2.in.params, standaloneSlaveParamsC.managers.head, standaloneMasterParamsC.clients.head)
      sanity1.sanityCheck(output1)

      val output2 = DRAMMonitor.getMonitoredTransactions().map(_.data).collect{ case t: TLBundleD => t}
      val sanity2 = new TLSanityChecker(TLBundleParameters(standaloneMasterParams, standaloneSlaveParams),
        standaloneSlaveParams.managers.head, standaloneMasterParams.clients.head)
      sanity2.sanityCheck(output2)

//      println("INNER (CORE)")
//      for (t <- L1Monitor.getMonitoredTransactions()) {
//        println(t)
//      }
//      println("OUTER (DRAM)")
//      for (t <- DRAMMonitor.getMonitoredTransactions()) {
//        println(t)
//      }
    }
  }
}