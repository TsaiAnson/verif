package verif

import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import chisel3.util.Queue
import chisel3._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import TestComponent._

class Playground extends AnyFlatSpec with ChiselScalatestTester {
  it should "run" in {
    val gen = UInt(8.W)
    val dut = () => new Queue(gen, 8, false, false)
    val initState = QueueTesterState.empty[UInt]
    val tester = new QueueTester(gen, 16, i => i.U)

    test(dut()).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { c =>
      val results = runSim(c.clock, c.io, tester, initState)
      val mPortTx = results.flatMap(_.masterPort)
      mPortTx.foreach(println(_))
      println()
      val sPortTx = results.flatMap(_.slavePort)
      sPortTx.foreach(println(_))
    }
  }
}
