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

class ArgsPassingTest extends AnyFlatSpec with CosimTester with BeforeAndAfterAllConfigMap with ChiselScalatestTester {
  var foo = ""

  override def beforeAll(configMap: ConfigMap) = {
    if (configMap.get("foo").isDefined) {
      foo = configMap.get("foo").fold("")(_.toString)
    }
  }

  it should "Get this test name" in {
    val i = new ImplicitDetailsPasser
    i.print
    println(foo)
  }
}
