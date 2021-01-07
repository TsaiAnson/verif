package verif

import chipsalliance.rocketchip.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest._
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}
import designs._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.WithoutTLMonitors
import chiseltest.experimental.TestOptionBuilder._
import verifTLUtils._
import TLTransaction._

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

  it should "Temp Sanity Test for New SDriver Skeleton" in {
    val TLCustomMaster = LazyModule(new VerifTLCustomMaster)
    test(TLCustomMaster.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      // Currently just recording requests, only Driver is needed
      val sDriver = new TLDriverSlave[HashMap[Int,Int]](c.clock, TLCustomMaster.out, HashMap[Int,Int](), testResponse)
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
    test(TLFeedback.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      // Drivers/Monitors
      val mDriver = new TLDriverMaster(c.clock, TLFeedback.in)
      val sDriver = new TLDriverSlave(c.clock, TLFeedback.out, HashMap[Int,Int](), testResponse)

      val monitor = new TLMonitor(c.clock, TLFeedback.in)

      val simCycles = 500

      implicit val params = TLFeedback.in.params
      val inputTransactions = Seq(
        Get(addr = 0x0),
        Put(addr = 0x0, data = 0x3333),
        Get(addr = 0x0),
        PutBurst(addr = 0x0, data = Seq(0x3333, 0x1234)),
        Get(addr = 0x0),
        Get(addr = 0x8),
        Logic(param = 2, addr = 0x0, data = 0x0),
        Get(addr = 0x0),
        LogicBurst(param = 2, addr = 0x0, data = Seq(0x0, 0x0)),
        Get(addr = 0x0),
        Get(addr = 0x8),
        Arith(param = 4, addr = 0x0, data = 0x0),
        Get(addr = 0x0),
        ArithBurst(param = 4, addr = 0x0, data = Seq(0x0, 0x0)),
        Get(addr = 0x0),
        Get(addr = 0x8)
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