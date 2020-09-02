package verif

import org.scalatest._
import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, VerilatorBackendAnnotation, WriteVcdAnnotation}
import designs.{CAMIO, ParameterizedCAMAssociative}
import chisel3.experimental.BundleLiterals._

case class InnerBundle[T <: Data](data: T, numb1: UInt = 0.U, numb2: UInt = 0.U) extends Bundle

case class NestedBundleTx[T <: Data](data: T, inner1: InnerBundle[UInt], inner2: InnerBundle[UInt], numb1: UInt = 0.U) extends Transaction

class RandomTest extends FlatSpec with ChiselScalatestTester {
  it should "random test basic" in {
    test(new ParameterizedCAMAssociative(8,8,8))
      .withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
        // Testing with non-nested basic transactions
        val CTx = CAMIO(8, 8)
        for (_ <- 0 to 9) {
          CTx.rand()
          for (field <- CTx.getClass.getDeclaredFields) {
            field.setAccessible(true)
            print(field.getName, field.get(CTx)); print(" ")
          }
          println("")
        }

        val DTx = DecoupledTX(165.U,0.U,1.U)
        for (_ <- 0 to 9) {
          DTx.rand()
          for (field <- DTx.getClass.getDeclaredFields) {
            field.setAccessible(true)
            print(field.getName, field.get(DTx)); print(" ")
          }
          println("")
        }
        assert(true)
      }
  }

  it should "random test nested" in {
    test(new ParameterizedCAMAssociative(8,8,8))
      .withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
        // Testing with single nested transactions
        val NTx = NestedBundleTx(100.U, InnerBundle(1.U,1.U,1.U), InnerBundle(2.U,2.U,2.U), 3.U)
        for (_ <- 0 to 9) {
          NTx.rand()
          for (field <- NTx.getClass.getDeclaredFields) {
            field.setAccessible(true)
            field.get(NTx).asInstanceOf[Any] match {
              case bundle: Bundle =>
                for (field1 <- bundle.getClass.getDeclaredFields) {
                  field1.setAccessible(true)
                  print(field.getName, field1.getName, field1.get(bundle)); print(" ")
                }
              case _: Any =>
                print(field.getName, field.get(NTx)); print(" ")
            }
          }
          println("")
        }
        assert(true)
      }
  }
}