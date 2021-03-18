package verif

import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal._
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule}
import TLTransaction._
import chisel3.Clock
import freechips.rocketchip.tilelink.{TLBundleA, TLBundleD, TLBundleParameters, TLChannel}
import scala.math.max

class TLRAMTest extends AnyFlatSpec with ChiselScalatestTester {
  def testRAM(dut: TLRAMStandalone, clock: Clock, stim: Seq[TLChannel], checker: Boolean = true): Seq[TLBundleD] = {
    val driver = new TLDriverMaster(clock, dut.in)
    val protocolChecker = if (checker) Some(new TLProtocolChecker(dut.in.params,
      dut.sParams, dut.mPortParams.masters.head)) else None
    val monitor = new TLMonitor(clock, dut.in, protocolChecker)
    val stimMonitor = new TLMonitor(clock, dut.in, None)
    val dispatcher = new TLUDispatcher(dut.in.params, None, stim)

    for (_ <- 0 until stim.length*10) {
      val seenTxns = stimMonitor.getMonitoredTransactions().map(_.data)
      val roundStim = dispatcher.next(seenTxns)
      driver.push(roundStim)
      clock.step(max(5, roundStim.length * 2))
    }

    monitor.getMonitoredTransactions().map(_.data).collect { case t: TLBundleD => t }
  }

  behavior of "TLRAMStandalone"
  it should "be testable via TLFuzzer" in {
    val dut = LazyModule(new TLRAMStandalone)
    test(dut.module).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val gen = new TLTransactionGenerator(dut.sParams, dut.in.params, overrideAddr = Some(AddressSet(0x00, 0x1ff)),
        burst = true, arith = true, logic = true)
      val txns = gen.generateTransactions(40)
      val output = testRAM(dut, c.clock, txns, false)
    }
  }

  it should "pass test with hardcoded burst" in {
    val dut = LazyModule(new TLRAMStandalone)
    test(dut.module).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      implicit val bundleParams: TLBundleParameters = dut.in.params
      val stim: Seq[TLBundleA] = Seq(
        Put(0x0, 0x3333),
        Get(0x0)
      ) ++
        PutBurst(0x10, Seq(0x1234, 0x5678), 0) :+
        Get(addr = 0x10)
      val output = testRAM(dut, c.clock, stim)
      val expectedOut = Seq(
        AccessAck(),
        AccessAckData(0x3333),
        AccessAck(4, 0),
        AccessAckData(0x1234)
      )
      val comparison = equalsTL(output, expectedOut)
      assert(comparison.isEmpty)
    }
  }

  it should "pass test with atomic transactions" in {
    val dut = LazyModule(new TLRAMStandalone)
    test(dut.module).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      implicit val bundleParams: TLBundleParameters = dut.in.params
      val stim = Seq(
          Put(addr = 0x0, data = 0x1234),
          Get(addr = 0x0),
          Arith(TLArithParam.ADD, 0x0, 0x1),
          Get(addr = 0x0),
          Logic(param = TLLogicParam.AND, addr = 0x0, data = 0xfff0),
          Get(addr = 0x0)
        )
      val output = testRAM(dut, c.clock, stim)
      println(output)
    }
  }

  // TODO: inject a cover for 4 consecutive writes in TLRAM and check that a 4 burst write stim hits it

  // This test just provides a reference output for the corresponding test in SlaveDriverTest
  it should "provide reference output for longer stimulus" in {
    val dut = LazyModule(new TLRAMStandalone)
    test(dut.module).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      implicit val bundleParams: TLBundleParameters = dut.in.params
      val stim = Seq(
        Get(0x0),
        Put(0x0, 0x3333),
        Get(0x0)
      ) ++
        (PutBurst(0x0, Seq(0x5555, 0x1234), 0) :+
          Get(0x0) :+
          Get(0x8)) ++
        (LogicBurst(TLLogicParam.AND, 0x0, Seq(0x0, 0x0)) :+
          Get(0x0) :+
          Get(0x8)) ++
        ArithBurst(TLArithParam.ADD, 0x0, Seq(0x2222, 0x8888)) :+
        Get(0x0) :+
        Get(0x8)
      val output = testRAM(dut, c.clock, stim, false)
      for (out <- output) {
        println(out.opcode, out.data, out.size)
      }
    }
  }

  it should "Test Non-aligned Address" in {
    val dut = LazyModule(new TLRAMStandalone)
    test(dut.module).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      implicit val bundleParams: TLBundleParameters = dut.in.params
      val stim = Seq(
//        Get(0x0),
        Put(0x0, 0x1111),
        Put(0x1, 0x3300, 0x2, 0x0, 0x0, false),
        Put(0x8, 0x0),
        Get(0x1, 0x0, 0x2, 0x0)
      )
      val output = testRAM(dut, c.clock, stim, false)
      println(s"Length: ${output.length}")
      for (out <- output) {
        println(out.opcode, out.data, out.size)
      }
    }
  }
}
