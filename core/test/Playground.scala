package verif

import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import chisel3.util.Queue
import chisel3._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import TestComponent._


// wrap chiseltest test() as an object that can be peeked and poked in a specific order, but only once, and can only be stepped externally
// potentially control combinational loops some other way (through principled IO = peek, (IO => poke), (peek => S), step routine)
class Playground extends AnyFlatSpec with ChiselScalatestTester {
  it should "run" in {
    val gen = UInt(8.W)
    val dut = () => new Queue(gen, 8, false, false)
    val initState = QueueTesterState.empty[UInt]
    val tester = new QueueTester(gen, 16, i => i.U)

    test(dut()).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val results = runSim(c.clock, c.io, tester, initState)
      val mPortTx = results.flatMap(_.masterPort)
      mPortTx.foreach(println(_))
      println()
      val sPortTx = results.flatMap(_.slavePort)
      sPortTx.foreach(println(_))
    }
  }
}
