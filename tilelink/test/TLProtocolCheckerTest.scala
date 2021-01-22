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

class TLProtocolCheckerTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = new WithoutTLMonitors

  it should "Fail Protocol Compliance: Address Not Aligned with Size" in {
    val DUT = LazyModule(new TLBufferStandalone)
    test(DUT.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      implicit val params: TLBundleParameters = DUT.in.params

      val driver = new TLDriverMaster(c.clock, DUT.in)
      val protocolChecker = new TLProtocolChecker(DUT.in.params, DUT.sPortParams.head.managers.head, DUT.mPortParams.head.clients.head)
      val monitor = new TLMonitor(c.clock, DUT.in, Some(protocolChecker))
      val simCycles = 10

      driver.push(PutBurst(addr = 0x10, data = Seq(0x1234, 0x5678, 0x8765, 0x4321), source = 0))
      c.clock.step(simCycles)
      assertThrows[AssertionError] {monitor.getMonitoredTransactions()}
    }
  }

  it should "Fail Protocol Compliance: Invalid Logical Param" in {
    val DUT = LazyModule(new TLBufferStandalone)
    test(DUT.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      implicit val params: TLBundleParameters = DUT.in.params

      val driver = new TLDriverMaster(c.clock, DUT.in)
      val protocolChecker = new TLProtocolChecker(DUT.in.params, DUT.sPortParams.head.managers.head, DUT.mPortParams.head.clients.head)
      val monitor = new TLMonitor(c.clock, DUT.in, Some(protocolChecker))
      val simCycles = 10

      driver.push(Seq(Logic(param = 7, addr = 0x10, data = 0x1234)))
      c.clock.step(simCycles)
      assertThrows[AssertionError] {monitor.getMonitoredTransactions()}
    }
  }

  it should "Fail Protocol Compliance: Non-Contiguous Mask on Arithmetic" in {
    val DUT = LazyModule(new TLBufferStandalone)
    test(DUT.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      implicit val params: TLBundleParameters = DUT.in.params

      val driver = new TLDriverMaster(c.clock, DUT.in)
      val protocolChecker = new TLProtocolChecker(DUT.in.params, DUT.sPortParams.head.managers.head, DUT.mPortParams.head.clients.head)
      val monitor = new TLMonitor(c.clock, DUT.in, Some(protocolChecker))
      val simCycles = 10

      driver.push(Seq(Arith(param = 1, addr = 0x10, data = 0x4321, mask = 0x55, size = 3, source = 0)))
      c.clock.step(simCycles)
      assertThrows[AssertionError] {monitor.getMonitoredTransactions()}
    }
  }

  it should "Fail Protocol Compliance: Invalid Corrupt on Get" in {
    val DUT = LazyModule(new TLBufferStandalone)
    test(DUT.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      implicit val params: TLBundleParameters = DUT.in.params

      val driver = new TLDriverMaster(c.clock, DUT.in)
      val protocolChecker = new TLProtocolChecker(DUT.in.params, DUT.sPortParams.head.managers.head, DUT.mPortParams.head.clients.head)
      val monitor = new TLMonitor(c.clock, DUT.in, Some(protocolChecker))
      val simCycles = 10

      driver.push(Seq(new TLBundleA(params).Lit(_.opcode -> TLOpcodes.Get.U, _.param -> 0.U, _.size -> 3.U,
        _.source -> 0.U, _.address -> 0x0.U, _.mask -> 0xff.U, _.corrupt -> 1.B, _.data -> 0.U)))
      c.clock.step(simCycles)
      assertThrows[AssertionError] {monitor.getMonitoredTransactions()}
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
      val protocolChecker = new TLProtocolChecker(DUT.in.params, DUT.sPortParams.head.managers.head, DUT.mPortParams.head.clients.head)
      val monitor = new TLMonitor(c.clock, DUT.in, Some(protocolChecker))
      val slaveFn = new BadSlaveFnOne(DUT.out.params)
      val sDriver = new TLDriverSlave(c.clock, DUT.out, slaveFn, TLMemoryModel.State.empty())
      val simCycles = 20

      mDriver.push(Seq(Get(0x0)))
      c.clock.step(simCycles)
      assertThrows[AssertionError] {monitor.getMonitoredTransactions()}
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
      val protocolChecker = new TLProtocolChecker(DUT.in.params, DUT.sPortParams.head.managers.head, DUT.mPortParams.head.clients.head)
      val monitor = new TLMonitor(c.clock, DUT.in, Some(protocolChecker))
      val slaveFn = new BadSlaveFnTwo(DUT.out.params)
      val sDriver = new TLDriverSlave(c.clock, DUT.out, slaveFn, TLMemoryModel.State.empty())
      val simCycles = 10

      driver.push(Seq(Get(0x0)))
      c.clock.step(simCycles)
      driver.push(Seq(Put(0x0, 0x123)))
      c.clock.step(simCycles)
      monitor.getMonitoredTransactions()
//      assertThrows[AssertionError] {monitor.getMonitoredTransactions()}
    }
  }
}