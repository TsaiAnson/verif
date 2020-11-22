package verif

import org.scalatest._
import chisel3._
import chiseltest._
import scala.util.Random

import VerifTLUtils._

class TLVIPTest extends FlatSpec with ChiselScalatestTester {
  it should "Test Transaction MIXIN Equality" in {
    val randGen = new Random()
    var randomUInt = 0.U
    var randomAddrUInt = 0.U

    // Checking GET
    for (_ <- 0 to 500) {
      randomAddrUInt = randGen.nextInt(Int.MaxValue).U
      randomUInt = randGen.nextInt(Int.MaxValue).U
      // Checking identity
      val g = Get(addr = randomAddrUInt)
      assert(g == g)

      // Checking equality
      assert(g == Get(addr = randomAddrUInt))

      // Checking non-equality
      assert(g != Get(addr = randGen.nextInt(Int.MaxValue).U))
      assert(g != PutFull(addr = randomAddrUInt, data = randomUInt))
      assert(g != AccessAck())
      assert(g != AccessAckData(data = randomUInt))
    }

    // Checking FullPut
    for (_ <- 0 to 500) {
      randomAddrUInt = randGen.nextInt(Int.MaxValue).U
      randomUInt = randGen.nextInt(Int.MaxValue).U
      // Checking identity
      val fp = PutFull(addr = randomAddrUInt, data = randomUInt)
      assert(fp == fp)

      // Checking equality
      assert(fp == PutFull(addr = randomAddrUInt, data = randomUInt))

      // Checking non-equality
      assert(fp != PutFull(addr = randGen.nextInt(Int.MaxValue).U, data = randGen.nextInt(Int.MaxValue).U))
      assert(fp != Get(addr = randomAddrUInt))
      assert(fp != AccessAck)
      assert(fp != AccessAckData(data = randomUInt))
    }

    // Checking AccessAck
    for (_ <- 0 to 10) {
      randomAddrUInt = randGen.nextInt(Int.MaxValue).U
      randomUInt = randGen.nextInt(Int.MaxValue).U
      val ack = AccessAck()

      // Checking identity
      assert(ack == ack)

      // Checking equality
      assert(ack == AccessAck())

      // Checking non-equality
      assert(ack != Get(addr = randomAddrUInt))
      assert(ack != PutFull(addr = randomAddrUInt, data = randomUInt))
      assert(ack != AccessAckData(data = randomUInt))
      assert(ack != AccessAckData(data = 0.U))
    }

    // Checking AccessAckData
    for (_ <- 0 to 500) {
      randomAddrUInt = randGen.nextInt(Int.MaxValue).U
      randomUInt = randGen.nextInt(Int.MaxValue).U
      // Checking identity
      val ackd = AccessAckData(data = randomUInt)
      assert(ackd == ackd)

      // Checking equality
      assert(ackd == AccessAckData(data = randomUInt))

      // Checking non-equality
      assert(ackd != AccessAckData(data = randGen.nextInt(Int.MaxValue).U))
      assert(ackd != Get(addr = randomUInt))
      assert(ackd != PutFull(addr = randomAddrUInt, data = randomUInt))
      assert(ackd != AccessAck)

    }
  }

  it should "Basic Unittest Burst TLTransaction to TLBundle conversion" in {
    val results = TLTransactiontoTLBundles(PutFullBurst(addr = 0x0.U, masks = List(0xff.U, 0x7f.U), datas = List(0x1234.U(64.W), 0x9876.U(64.W)), size = 4.U))
    for (tnx <- results) {
      println(tnx)
    }
  }
}