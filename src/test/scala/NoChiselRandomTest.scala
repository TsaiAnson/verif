package verif

import org.scalatest._

import chisel3._
import designs.CAMIO

class zeroBundle extends Bundle {
  val empty: UInt = UInt(0.W)
}

case class InnerBundleNC[T <: Data](data: T, numb1: SInt = 0.S(8.W), numb2: UInt = 0.U(8.W)) extends Bundle

case class NestedBundleTxNC[T <: Data](data: T, inner1: InnerBundleNC[UInt], inner2: InnerBundleNC[UInt], numb1: UInt = 0.U) extends Transaction

case class TestBundleTxNC (testB: Bundle) extends Transaction

class NoChiselRandomTest extends FlatSpec with Matchers {
  "Basic NoChiselRandomTest" should "have no error" in {
    // Testing Non-nested Structures
    val CTx = CAMIO(8, 8)
    for (_ <- 0 to 9) {
      CTx.rand
      CTx.listContents
    }

    val DTx = DecoupledTX(165.U, 0.U, 1.U)
    for (_ <- 0 to 9) {
      DTx.rand
      DTx.listContents
    }
  }

  "Nested NoChiselRandomTest" should "have no error" in {
    // Testing Nested Structures
    // Testing with single nested transactions
    val NTx = NestedBundleTxNC(100.U, InnerBundleNC(1.U,10.S,1.U), InnerBundleNC(2.U,2.S,2.U), 3.U)
    for (_ <- 0 to 9) {
      NTx.rand
      NTx.listContents
    }
  }

  // Testing empty bundles, bundles with uint of width 0
  "Other Bundle Edge Cases" should "have no error" in {
    val EBTx = TestBundleTxNC(new Bundle{})
    for (_ <- 0 to 9) {
      EBTx.rand
      EBTx.listContents
    }

    val ZBTx = TestBundleTxNC(new zeroBundle)
    for (_ <- 0 to 9) {
      ZBTx.rand
      ZBTx.listContents
    }
  }
}