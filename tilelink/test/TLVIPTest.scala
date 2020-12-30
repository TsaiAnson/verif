package verif

import chipsalliance.rocketchip.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest._
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import designs.{VerifTLCustomMaster, VerifTLMasterSlaveFeedback}
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.WithoutTLMonitors
import chiseltest.experimental.TestOptionBuilder._
import verifTLUtils._
import scala.collection.mutable.HashMap

class TLVIPTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = new WithoutTLMonitors

  // Will fix up once I figure out bug with new Burst transactions
//  it should "Test Transaction MIXIN Equality" in {
//    val randGen = new Random()
//    var randomUInt = 0.U
//    var randomAddrUInt = 0.U
//
//    // Checking GET
//    for (_ <- 0 to 500) {
//      randomAddrUInt = randGen.nextInt(Int.MaxValue).U
//      randomUInt = randGen.nextInt(Int.MaxValue).U
//      // Checking identity
//      val g = Get(addr = randomAddrUInt)
//      assert(g == g)
//
//      // Checking equality
//      assert(g == Get(addr = randomAddrUInt))
//
//      // Checking non-equality
//      assert(g != Get(addr = randGen.nextInt(Int.MaxValue).U))
//      assert(g != PutFull(addr = randomAddrUInt, data = randomUInt))
//      assert(g != AccessAck())
//      assert(g != AccessAckData(data = randomUInt))
//    }
//
//    // Checking FullPut
//    for (_ <- 0 to 500) {
//      randomAddrUInt = randGen.nextInt(Int.MaxValue).U
//      randomUInt = randGen.nextInt(Int.MaxValue).U
//      // Checking identity
//      val fp = PutFull(addr = randomAddrUInt, data = randomUInt)
//      assert(fp == fp)
//
//      // Checking equality
//      assert(fp == PutFull(addr = randomAddrUInt, data = randomUInt))
//
//      // Checking non-equality
//      assert(fp != PutFull(addr = randGen.nextInt(Int.MaxValue).U, data = randGen.nextInt(Int.MaxValue).U))
//      assert(fp != Get(addr = randomAddrUInt))
//      assert(fp != AccessAck)
//      assert(fp != AccessAckData(data = randomUInt))
//    }
//
//    // Checking AccessAck
//    for (_ <- 0 to 10) {
//      randomAddrUInt = randGen.nextInt(Int.MaxValue).U
//      randomUInt = randGen.nextInt(Int.MaxValue).U
//      val ack = AccessAck()
//
//      // Checking identity
//      assert(ack == ack)
//
//      // Checking equality
//      assert(ack == AccessAck())
//
//      // Checking non-equality
//      assert(ack != Get(addr = randomAddrUInt))
//      assert(ack != PutFull(addr = randomAddrUInt, data = randomUInt))
//      assert(ack != AccessAckData(data = randomUInt))
//      assert(ack != AccessAckData(data = 0.U))
//    }
//
//    // Checking AccessAckData
//    for (_ <- 0 to 500) {
//      randomAddrUInt = randGen.nextInt(Int.MaxValue).U
//      randomUInt = randGen.nextInt(Int.MaxValue).U
//      // Checking identity
//      val ackd = AccessAckData(data = randomUInt)
//      assert(ackd == ackd)
//
//      // Checking equality
//      assert(ackd == AccessAckData(data = randomUInt))
//
//      // Checking non-equality
//      assert(ackd != AccessAckData(data = randGen.nextInt(Int.MaxValue).U))
//      assert(ackd != Get(addr = randomUInt))
//      assert(ackd != PutFull(addr = randomAddrUInt, data = randomUInt))
//      assert(ackd != AccessAck)
//
//    }
//  }

  it should "Basic Unittest Burst TLTransaction to TLBundle conversion" in {
    val results = TLTransactiontoTLBundles(PutFullBurst(source = 0.U, addr = 0x0.U, masks = List(0xff.U, 0x7f.U),
      datas = List(0x1234.U(64.W), 0x9876.U(64.W)), size = 4.U))
    for (tnx <- results) {
      println(tnx)
    }
  }

  it should "Basic Unittest groupTLBundles" in {
    val results = groupTLBundles(List(TLUBundleAHelper(size = 4.U),TLUBundleAHelper(size = 4.U),
      TLUBundleDHelper(size = 5.U),TLUBundleDHelper(size = 5.U),TLUBundleDHelper(size = 5.U),
      TLUBundleDHelper(size = 5.U),TLUBundleAHelper(size = 3.U)))
    for (tnx <- results) {
      println(tnx)
      println()
    }
  }

  it should "Basic Unittest TLTransaction to TLBundles to TLTransaction" in {
    val testTxns = List(AccessAck(size = 3.U, denied = true.B), AccessAckData(size = 3.U, denied = false.B, data = 0x1.U(64.W)),
      AccessAckDataBurst(size = 4.U, denied = false.B, datas = List(0x02.U(64.W), 0x03.U(64.W))),
      PutFull(source = 0.U, addr = 0x10.U, mask = 0xff.U, data = 0x11.U(64.W)),
      PutFullBurst(size = 4.U, source = 0.U, addr = 0x0.U, masks = List(0xff.U, 0x7f.U), datas = List(0x1234.U(64.W), 0x9876.U(64.W))),
      Get(size = 3.U, source = 0.U, addr = 0x15.U, mask = 0xff.U), Get(size = 4.U, source = 0.U, addr = 0x20.U, mask = 0xff.U)
    )
    var i = 0
    for (txn <- testTxns) {
      println(s"Test ${i}, txn: ${txn}")
      val result = TLBundlestoTLTransaction(TLTransactiontoTLBundles(txn))
      println(s"Result: ${result}")
//      assert (txn == result)
      i += 1
    }

    println(PutFull(source = 0.U, addr = 0x10.U, mask = 0xff.U, data = 0x11.U(64.W)) == PutFull(source = 0.U, addr = 0x10.U, mask = 0xff.U, data = 0x11.U(64.W)))

    println(PutFullBurst(size = 4.U, source = 0.U, addr = 0x0.U, masks = List(0xff.U, 0x7f.U), datas = List(0x1234.U(64.W), 0x9876.U(64.W))) ==
      PutFullBurst(size = 4.U, source = 0.U, addr = 0x0.U, masks = List(0xff.U, 0x7f.U), datas = List(0x1234.U(64.W), 0x9876.U(64.W))))
  }

  it should "Temp Sanity Test for New SDriver Skeleton" in {
    val TLCustomMaster = LazyModule(new VerifTLCustomMaster)
    test(TLCustomMaster.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>

      // Currently just recording requests, only Driver is needed
      val sDriver = new TLDriverSlave(c.clock, TLCustomMaster.out, HashMap[Int,Int](), testResponse)
      val monitor = new TLMonitor(c.clock, TLCustomMaster.out)
      val simCycles = 80

      // State testing
      val init_state = HashMap[Int,Int]()
      sDriver.setState(init_state)

      c.clock.step(simCycles)

      val output = monitor.getMonitoredTransactions().toArray

      // Transactions
      for (out <- output) {
        println(out)
      }

      // State Map
      println("Resulting State")
      val hash = sDriver.getState()
      for (x <- hash.keys) {
        print(s"(${x}, ${hash(x)}), ")
      }
      println("")

      // Init State (making sure that the original state was not modified)
      println("Initial State")
      for (x <- init_state.keys) {
        print(s"(${x}, ${init_state(x)}), ")
      }
      println("")
    }
  }

  it should "Test for New SDriver" in {
    val TLFeedback = LazyModule(new VerifTLMasterSlaveFeedback)
    test(TLFeedback.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>

      // Drivers/Monitors
      val mDriver = new TLDriverMaster(c.clock, TLFeedback.in)
      val sDriver = new TLDriverSlave(c.clock, TLFeedback.out, HashMap[Int,Int](), testResponse)

      val monitor = new TLMonitor(c.clock, TLFeedback.in)

      val simCycles = 500

      val inputTransactions = Seq(
        Get(size = 3.U, source = 0.U, addr = 0x0.U, mask = 0xff.U),
        PutFull(source = 0.U, addr = 0x0.U, mask = 0xff.U, data = 0x3333.U),
        Get(size = 3.U, source = 0.U, addr = 0x0.U, mask = 0xff.U),
        PutFullBurst(size = 4.U, source = 0.U, addr = 0x0.U, masks = List(0xff.U, 0xff.U), datas = List(0x3333.U, 0x1234.U)),
        Get(size = 3.U, source = 0.U, addr = 0x0.U, mask = 0xff.U),
        Get(size = 3.U, source = 0.U, addr = 0x8.U, mask = 0xff.U),
        LogicData(param = 2.U, source = 0.U, addr = 0x0.U, mask = 0xff.U, data = 0.U),
        Get(size = 3.U, source = 0.U, addr = 0x0.U, mask = 0xff.U),
        LogicDataBurst(param = 2.U, source = 0.U, addr = 0x0.U, size = 4.U, masks = List(0xff.U, 0xff.U), datas = List(0.U, 0.U)),
        Get(size = 3.U, source = 0.U, addr = 0x0.U, mask = 0xff.U),
        Get(size = 3.U, source = 0.U, addr = 0x8.U, mask = 0xff.U),
        ArithData(param = 4.U, source = 0.U, addr = 0x0.U, mask = 0xff.U, data = 0x8000.U),
        Get(size = 3.U, source = 0.U, addr = 0x0.U, mask = 0xff.U),
        Get(size = 3.U, source = 0.U, addr = 0x8.U, mask = 0xff.U),
        ArithDataBurst(param = 4.U, source = 0.U, addr = 0x0.U, size = 4.U, masks = List(0xff.U, 0xff.U), datas = List(0x1234.U, 0x3333.U)),
      )

      mDriver.push(inputTransactions)
      c.clock.step(simCycles)

      val output = monitor.getMonitoredTransactions(filterD).toArray

      // Transactions
      for (out <- output) {
        println(out)
      }

      // State Map
      val hash = sDriver.getState()
      for (x <- hash.keys) {
        print(s"(${x}, ${hash(x)}), ")
      }
      println("")
    }
  }
}