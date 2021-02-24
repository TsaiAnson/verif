package verif

import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import chisel3.util.Queue
import chisel3._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation

import TestComponent._

class Playground extends AnyFlatSpec with ChiselScalatestTester {
  it should "run" in {
    val gen = UInt(8.W)
    val dut = () => new Queue(gen, 8, false, false)
    val initState = QueueTesterState.withStim(gen, i => i.U)
    val tester = new QueueTester(gen)

    test(dut()).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val results = runSim(c.clock, c.io, tester, initState)
      val mPortTx = results.map(_.masterPort).flatten
      mPortTx.foreach(println(_))
      println()
      val sPortTx = results.map(_.slavePort).flatten
      sPortTx.foreach(println(_))
    }
  }
}
