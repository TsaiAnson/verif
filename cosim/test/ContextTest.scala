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


class ImplicitDetailsPasser(implicit val cosimTestDetails: CosimTestDetails) {
  def print(): Unit = {
    println(cosimTestDetails.testName.getOrElse("No name found"))
    println(cosimTestDetails.sbtRoot.getOrElse("No sbt root root found"))
  }
}

class ContextTest extends AnyFlatSpec with CosimTester with ChiselScalatestTester {
  it should "Get this test name" in {
    val i = new ImplicitDetailsPasser
    i.print

    test(new MultiIOModule {}).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      assert(true)
    }
  }
}
