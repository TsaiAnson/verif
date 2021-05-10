package verif

import org.scalatest.flatspec.AnyFlatSpec

import designs._
import cosim._
import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.experimental._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import org.scalatest._
import org.scalatest.TestSuite

class ArgsPassingTest extends AnyFlatSpec with CosimTester with ChiselScalatestTester {
  it should "Get the simTarget argument" in {
    println(simTarget)
  }
}
