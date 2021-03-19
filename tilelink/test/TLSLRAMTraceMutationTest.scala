package verif

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import SL._
import chisel3.experimental.BundleLiterals._
import TLTransaction._
import chipsalliance.rocketchip.config.Parameters
import chiseltest.internal.WriteVcdAnnotation
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule}
import freechips.rocketchip.subsystem.WithoutTLMonitors
import freechips.rocketchip.tilelink.{TLBundleA, TLBundleD, TLBundleParameters, TLChannel}
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable.{HashMap, ListBuffer}
import scala.math.max

class TLSLRAMTraceMutationTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val params: TLBundleParameters = TLBundleParameters(DefaultTLParams.master(), DefaultTLParams.slave)
  val r = scala.util.Random

  def testRAM(dut: TLRAMNoModelStandalone, clock: Clock, stim: Seq[TLChannel]): Seq[TLChannel] = {
    val driver = new TLDriverMaster(clock, dut.in)
    val monitor = new TLMonitor(clock, dut.in)
    val stimMonitor = new TLMonitor(clock, dut.in, None)
    val dispatcher = new TLUDispatcher(dut.in.params, None, stim)

    for (_ <- 0 until stim.length*4) {
      val seenTxns = stimMonitor.getMonitoredTransactions().map(_.data)
      val roundStim = dispatcher.next(seenTxns)
      driver.push(roundStim)
      clock.step(max(5, roundStim.length * 2))
    }

    monitor.getMonitoredTransactions().map(_.data)
  }

  def mutateTLBundle(txn: TLChannel): TLChannel = {
    txn match {
      case t: TLBundleA =>
        // Should be able to mutate any field, but for now just address
        new TLBundleA(params).Lit(_.opcode -> t.opcode, _.param -> t.param, _.size -> t.size, _.source -> t.source,
          _.address -> (t.address.litValue() + r.nextInt(100)).U, _.mask -> t.mask, _.corrupt -> t.corrupt, _.data -> t.data)
      case t: TLBundleD =>
        // Should be able to mutate any field, but for now just data + opcode (in case data is ignored)
        new TLBundleD(params).Lit(_.opcode -> (t.opcode.litValue() & r.nextInt(2)).U, _.param -> t.param, _.size -> t.size, _.source -> t.source,
          _.sink -> t.sink, _.denied -> t.denied, _.corrupt -> t.corrupt, _.data -> (t.data.litValue() + r.nextInt(100)).U)
    }
  }

  // Saving good/bad transaction traces for last 2 tests
  var goodTrace = Seq[TLChannel]()
  var badTrace = Seq[TLChannel]()

  it should "Test TLRAM Trace Mutation: Adding extra transactions" in {
    val dut = LazyModule(new TLRAMNoModelStandalone)
    test(dut.module).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val gen = new TLTransactionGenerator(dut.sPortParams, dut.in.params, overrideAddr = Some(AddressSet(0x100, 0xff)),
        burst = true, arith = true, logic = true)
      val txns = gen.generateTransactions(40)
      val protocolChecker = new TLSLProtocolChecker(dut.mPortParams, dut.bridge.edges.out.head.slave)

      val txnTrace = testRAM(dut, c.clock, txns)

      // Checking that output is good to begin with
      assert(protocolChecker.check(txnTrace, Some(new TLSLMemoryModel(TLBundleParameters(dut.mPortParams, dut.bridge.edges.out.head.slave)))))
      goodTrace = txnTrace

      // Add extra request (Put)
      val mutTxnTrace = ListBuffer[TLChannel]()
      txnTrace.copyToBuffer(mutTxnTrace)
      val idx = r.nextInt(txnTrace.size)
      mutTxnTrace.insert(idx, Put(0x0,0x1234))
      badTrace = mutTxnTrace

      // Check that mutated transaction trace fails
      assert(!protocolChecker.check(mutTxnTrace, Some(new TLSLMemoryModel(TLBundleParameters(dut.mPortParams, dut.bridge.edges.out.head.slave)))))
    }
  }

  it should "Test TLRAM Trace Mutation: Removing transactions" in {
    val dut = LazyModule(new TLRAMNoModelStandalone)
    test(dut.module).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val gen = new TLTransactionGenerator(dut.sPortParams, dut.in.params, overrideAddr = Some(AddressSet(0x100, 0xff)),
        burst = true, arith = true, logic = true)
      val txns = gen.generateTransactions(40)

      val protocolChecker = new TLSLProtocolChecker(dut.mPortParams, dut.bridge.edges.out.head.slave)

      val txnTrace = testRAM(dut, c.clock, txns)

      // Checking that output is good to begin with
      assert(protocolChecker.check(txnTrace, Some(new TLSLMemoryModel(TLBundleParameters(dut.mPortParams, dut.bridge.edges.out.head.slave)))))

      // Removing transaction
      val mutTxnTrace = ListBuffer[TLChannel]()
      txnTrace.copyToBuffer(mutTxnTrace)
      val idx = r.nextInt(txnTrace.size)
      mutTxnTrace.remove(idx)

      // Check that mutated transaction trace fails
      assert(!protocolChecker.check(mutTxnTrace, Some(new TLSLMemoryModel(TLBundleParameters(dut.mPortParams, dut.bridge.edges.out.head.slave)))))
    }
  }

  it should "Test TLRAM Trace Mutation: Modifying transaction fields" in {
    val dut = LazyModule(new TLRAMNoModelStandalone)
    test(dut.module).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val gen = new TLTransactionGenerator(dut.sPortParams, dut.in.params, overrideAddr = Some(AddressSet(0x100, 0xff)),
        burst = true, arith = true, logic = true)
      val txns = gen.generateTransactions(40)
      val protocolChecker = new TLSLProtocolChecker(dut.mPortParams, dut.bridge.edges.out.head.slave)

      val txnTrace = testRAM(dut, c.clock, txns)

      // Checking that output is good to begin with
      assert(protocolChecker.check(txnTrace, Some(new TLSLMemoryModel(TLBundleParameters(dut.mPortParams, dut.bridge.edges.out.head.slave)))))

      // Changing transaction fields
      val mutTxnTrace = ListBuffer[TLChannel]()
      txnTrace.copyToBuffer(mutTxnTrace)

      // Mutate few transactions (at least 2)
      for (_ <- 0 until (r.nextInt(4) + 2)) {
        val idx = r.nextInt(txnTrace.size)
        mutTxnTrace.update(idx, mutateTLBundle(mutTxnTrace(idx)))
      }

      // Check that mutated transaction trace fails
      assert(!protocolChecker.check(mutTxnTrace, Some(new TLSLMemoryModel(TLBundleParameters(dut.mPortParams, dut.bridge.edges.out.head.slave)))))
    }
  }

  it should "Test TLRAM Trace Mutation: Concat 2 good transaction traces (separate address space)" in {
    val dut = LazyModule(new TLRAMNoModelStandalone)
    test(dut.module).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      // NOTE: Must have different address space or else might fail (separate traces have different data models)
      val gen = new TLTransactionGenerator(dut.sPortParams, dut.in.params, overrideAddr = Some(AddressSet(0x200, 0xff)),
        burst = true, arith = true, logic = true)
      val protocolChecker = new TLSLProtocolChecker(dut.mPortParams, dut.bridge.edges.out.head.slave)

      val txns = gen.generateTransactions(10)
      val txnTrace = testRAM(dut, c.clock, txns)
      // Checking that output is good to begin with
      assert(protocolChecker.check(txnTrace, Some(new TLSLMemoryModel(TLBundleParameters(dut.mPortParams, dut.bridge.edges.out.head.slave)))))
      // Assert that good trace is still good
      assert(protocolChecker.check(goodTrace, Some(new TLSLMemoryModel(TLBundleParameters(dut.mPortParams, dut.bridge.edges.out.head.slave)))))

      val combTxnTrace = goodTrace ++ txnTrace
      // Check that combined txn trace passes
      assert(protocolChecker.check(combTxnTrace, Some(new TLSLMemoryModel(TLBundleParameters(dut.mPortParams, dut.bridge.edges.out.head.slave)))))
    }
  }

  it should "Test TLRAM Trace Mutation: Concat good and bad transaction trace (separate address space)" in {
    val dut = LazyModule(new TLRAMNoModelStandalone)
    test(dut.module).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val gen = new TLTransactionGenerator(dut.sPortParams, dut.in.params, overrideAddr = Some(AddressSet(0x200, 0xff)),
        burst = true, arith = true, logic = true)
      val protocolChecker = new TLSLProtocolChecker(dut.mPortParams, dut.bridge.edges.out.head.slave)

      val txns = gen.generateTransactions(40)
      val txnTrace = testRAM(dut, c.clock, txns)
      // Checking that output is good to begin with
      assert(protocolChecker.check(txnTrace, Some(new TLSLMemoryModel(TLBundleParameters(dut.mPortParams, dut.bridge.edges.out.head.slave)))))
      // Assert that bad trace is still bad
      assert(!protocolChecker.check(badTrace, Some(new TLSLMemoryModel(TLBundleParameters(dut.mPortParams, dut.bridge.edges.out.head.slave)))))

      val combTxnTrace = badTrace ++ txnTrace
      // Check that combined txn trace fails
      assert(!protocolChecker.check(combTxnTrace, Some(new TLSLMemoryModel(TLBundleParameters(dut.mPortParams, dut.bridge.edges.out.head.slave)))))
    }
  }
}