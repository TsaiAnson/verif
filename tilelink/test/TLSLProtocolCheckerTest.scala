package verif

import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule}
import freechips.rocketchip.subsystem.WithoutTLMonitors
import freechips.rocketchip.tilelink.{TLBundleA, TLBundleD, TLBundleParameters, TLChannel}
import TLTransaction._
import chisel3._
import chisel3.experimental.BundleLiterals._

class TLSLProtocolCheckerTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = new WithoutTLMonitors

  it should "Pass Protocol Compliance" in {
    val DUT = LazyModule(new TLRAMNoModelStandalone)
    test(DUT.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      implicit val params: TLBundleParameters = DUT.in.params

      val driver = new TLDriverMaster(c.clock, DUT.in)
      val protocolChecker = new TLSLProtocolChecker(DUT.mPortParams, DUT.sPortParams)
      val monitor = new TLMonitor(c.clock, DUT.in)
      val simCycles = 10

      val stim = Seq(
        Get(0x0), // Normal Get
        Put(0x0, 0x123456789L), // Ordinary Put
        Get(0x1, 0x0, 0x20, 0), // Non-wordaligned address, Non-aligned mask
        Put(0x4, 0x00889900, Integer.parseInt("0110", 2), 0x2, 0x0, false),
        Get(0x0) // Normal Get
      )
      driver.push(stim)

      c.clock.step(simCycles)
      assert(protocolChecker.check(monitor.getMonitoredTransactions().map(_.data)))
    }
  }

  it should "Fail Protocol Compliance: Address Not Aligned with Size" in {
    val DUT = LazyModule(new TLRAMNoModelStandalone)
    test(DUT.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      implicit val params: TLBundleParameters = DUT.in.params

      val driver = new TLDriverMaster(c.clock, DUT.in)
      val protocolChecker = new TLSLProtocolChecker(DUT.mPortParams, DUT.sPortParams)
      val monitor = new TLMonitor(c.clock, DUT.in)
      val simCycles = 10

      driver.push(PutBurst(addr = 0x10, data = Seq(0x1234, 0x5678, 0x8765, 0x4321), source = 0))
      c.clock.step(simCycles)
      assert(!protocolChecker.check(monitor.getMonitoredTransactions().map(_.data)))
    }
  }

  it should "Fail Protocol Compliance: Non-Contiguous Mask on Arithmetic" in {
    val DUT = LazyModule(new TLRAMNoModelStandalone)
    test(DUT.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      implicit val params: TLBundleParameters = DUT.in.params

      val driver = new TLDriverMaster(c.clock, DUT.in)
      val protocolChecker = new TLSLProtocolChecker(DUT.mPortParams, DUT.sPortParams)
      val monitor = new TLMonitor(c.clock, DUT.in)
      val simCycles = 10

      driver.push(Seq(Arith(TLArithParam.MIN, 0x10, 0x4321, 0x55, 3, 0)))
      c.clock.step(simCycles)
      assert(!protocolChecker.check(monitor.getMonitoredTransactions().map(_.data)))
    }
  }

  it should "Fail Protocol Compliance: Invalid Corrupt on Get" in {
    val DUT = LazyModule(new TLRAMNoModelStandalone)
    test(DUT.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      implicit val params: TLBundleParameters = DUT.in.params

      val driver = new TLDriverMaster(c.clock, DUT.in)
      val protocolChecker = new TLSLProtocolChecker(DUT.mPortParams, DUT.sPortParams)
      val monitor = new TLMonitor(c.clock, DUT.in)
      val simCycles = 10

      driver.push(Seq(new TLBundleA(params).Lit(_.opcode -> TLOpcodes.Get.U, _.param -> 0.U, _.size -> 3.U,
        _.source -> 0.U, _.address -> 0x0.U, _.mask -> 0xff.U, _.corrupt -> 1.B, _.data -> 0.U)))
      c.clock.step(simCycles)
      assert(!protocolChecker.check(monitor.getMonitoredTransactions().map(_.data)))
    }
  }

  // Temporary (Invalid) Response functions - Invalid Denied AccessAckData
  class BadSlaveFnOne (p: TLBundleParameters) extends TLSlaveFunction[TLMemoryModel.State] {
    implicit val params: TLBundleParameters = p
    override def response(tx: TLChannel, state: TLMemoryModel.State): (Seq[TLChannel], TLMemoryModel.State) = {
      tx match { case _ => (Seq(new TLBundleD(params).Lit(_.opcode -> TLOpcodes.AccessAckData.U, _.param -> 0.U, _.size -> 3.U,
        _.source -> 0.U, _.denied -> 1.B, _.corrupt -> 0.B, _.data -> 0.U)), state)}}
  }

  it should "Fail Protocol Compliance: AccessAckData Response Denied but Not Corrupt" in {
    val DUT = LazyModule(new TLBufferStandalone)
    test(DUT.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      implicit val params: TLBundleParameters = DUT.in.params

      val mDriver = new TLDriverMaster(c.clock, DUT.in)
      val protocolChecker = new TLSLProtocolChecker(DUT.mPortParams, DUT.sPortParams)
      val monitor = new TLMonitor(c.clock, DUT.in)
      val slaveFn = new BadSlaveFnOne(DUT.out.params)
      val sDriver = new TLDriverSlave(c.clock, DUT.out, slaveFn, TLMemoryModel.State.empty())
      val simCycles = 20

      mDriver.push(Seq(Get(0x0)))
      c.clock.step(simCycles)
      assert(!protocolChecker.check(monitor.getMonitoredTransactions().map(_.data)))
    }
  }

  // Temporary (Invalid) Response functions - No Acks Sent
  class BadSlaveFnTwo (p: TLBundleParameters) extends TLSlaveFunction[TLMemoryModel.State] {
    implicit val params: TLBundleParameters = p
    override def response(tx: TLChannel, state: TLMemoryModel.State): (Seq[TLChannel], TLMemoryModel.State) = {
      tx match { case _ => (Seq(), state)}}
  }

  it should "Fail Protocol Compliance: No AccessAck Response" in {
    val DUT = LazyModule(new TLBufferStandalone)
    test(DUT.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      implicit val params: TLBundleParameters = DUT.in.params

      val driver = new TLDriverMaster(c.clock, DUT.in)
      val protocolChecker = new TLSLProtocolChecker(DUT.mPortParams, DUT.sPortParams)
      val monitor = new TLMonitor(c.clock, DUT.in)
      val slaveFn = new BadSlaveFnTwo(DUT.out.params)
      val sDriver = new TLDriverSlave(c.clock, DUT.out, slaveFn, TLMemoryModel.State.empty())
      val simCycles = 10

      driver.push(Seq(Get(0x0)))
      c.clock.step(simCycles)
      driver.push(Seq(Put(0x0, 0x123)))
      c.clock.step(simCycles)
      assert(!protocolChecker.check(monitor.getMonitoredTransactions().map(_.data)))
    }
  }
}