package verif

import org.scalatest._

import chisel3._
import designs.CAMIO

class zeroBundle extends Bundle {
  val empty: UInt = UInt(0.W)
}

case class InnerBundleNC[T <: Data](data: T, numb1: SInt = 0.S(8.W), numb2: UInt = 0.U(8.W)) extends Bundle

case class NestedBundleTxNC[T <: Data](data: T, inner1: InnerBundleNC[UInt], inner2: InnerBundleNC[UInt],
                                       numb1: UInt = 0.U) (implicit val r: VerifRandomGenerator) extends Transaction

case class TestBundleTxNC (testB: Bundle) (implicit val r: VerifRandomGenerator) extends Transaction

case class TestVecBundle[T <: Data](data: T, vec1: Vec[T]) (implicit val r: VerifRandomGenerator) extends Transaction

class NoChiselRandomTest extends FlatSpec with Matchers {
  implicit val randGen: VerifRandomGenerator = new ScalaVerifRandomGenerator

  "Basic NoChiselRandomTest" should "have no error" in {
    // Testing Non-nested Structures
    val CTx = CAMIO(8, 8)
    for (_ <- 0 to 2) {
      CTx.rand.printContents
    }

    println("")

    val DTx = DecoupledTX(165.U, 0.U, 1.U)
    for (_ <- 0 to 9) {
      DTx.rand.printContents
    }
  }

  "Nested NoChiselRandomTest" should "have no error" in {
    // Testing Nested Structures
    // Testing with single nested transactions
    val NTx = NestedBundleTxNC(100.U, InnerBundleNC(1.U,10.S,1.U), InnerBundleNC(2.U,2.S,2.U), 3.U)
    for (_ <- 0 to 9) {
      NTx.rand.printContents
    }
  }

  // Testing empty bundles, bundles with uint of width 0
  "Other Bundle Edge Cases" should "have no error" in {
    val EBTx = TestBundleTxNC(new Bundle{})
    for (_ <- 0 to 9) {
      EBTx.rand.printContents
    }

    val ZBTx = TestBundleTxNC(new zeroBundle)
    for (_ <- 0 to 9) {
      ZBTx.rand.printContents
    }
  }

  "Deterministic Testing" should "have no error" in {
    var out1 = ""
    var out2 = ""

    // Testing that two CAMIO's with the same seed should have deterministic rand
    var CTx = CAMIO(8, 8)
    randGen.setSeed(1234567890.toLong)
    for (_ <- 0 to 9) {
      out1 += CTx.rand.getStringContents
    }

    CTx = CAMIO(8, 8)
    randGen.setSeed(1234567890.toLong)
    for (_ <- 0 to 9) {
      out2 += CTx.rand.getStringContents
    }

//    print(out1)
//    println("DEBUG")
//    print(out2)

    (out1 == out2) should be (true)

    // Testing that two CAMIO's with the different seed should have deterministic rand
    out1 = ""
    out2 = ""
    CTx = CAMIO(8, 8)
    randGen.setSeed(1234567890.toLong)
    for (_ <- 0 to 9) {
      out1 += CTx.rand.getStringContents
    }

    CTx = CAMIO(8, 8)
    randGen.setSeed(987654321.toLong)
    for (_ <- 0 to 9) {
      out2 += CTx.rand.getStringContents
    }

    (out1 == out2) should be (false)

    // Performing the above tests on DecoupledTX to double check
    out1 = ""
    out2 = ""
    var DTx = DecoupledTX(165.U, 0.U, 1.U)
    randGen.setSeed(123123123.toLong)
    for (_ <- 0 to 9) {
      out1 += DTx.rand.getStringContents
    }

    DTx = DecoupledTX(165.U, 0.U, 1.U)
    randGen.setSeed(123123123.toLong)
    for (_ <- 0 to 9) {
      out2 += DTx.rand.getStringContents
    }

//    print(out1)
//    println("DEBUG")
//    print(out2)

    (out1 == out2) should be (true)

    out1 = ""
    out2 = ""
    DTx = DecoupledTX(165.U, 0.U, 1.U)
    randGen.setSeed(111111111.toLong)
    for (_ <- 0 to 9) {
      out1 += DTx.rand.getStringContents
    }

    DTx = DecoupledTX(165.U, 0.U, 1.U)
    randGen.setSeed(222222222.toLong)
    for (_ <- 0 to 9) {
      out2 += DTx.rand.getStringContents
    }

    (out1 == out2) should be (false)

    // Performing the above tests on the Nested Transactions to triple check
    out1 = ""
    out2 = ""
    var NTx = NestedBundleTxNC(255.U, InnerBundleNC(255.U,255.S,255.U), InnerBundleNC(255.U,255.S,255.U), 255.U)
    randGen.setSeed(676767.toLong)
    for (_ <- 0 to 9) {
      out1 += NTx.rand.getStringContents
    }

    NTx = NestedBundleTxNC(255.U, InnerBundleNC(255.U,255.S,255.U), InnerBundleNC(255.U,255.S,255.U), 255.U)
    randGen.setSeed(676767.toLong)
    for (_ <- 0 to 9) {
      out2 += NTx.rand.getStringContents
    }

//    print(out1)
//    println("DEBUG")
//    print(out2)

    (out1 == out2) should be (true)

    out1 = ""
    out2 = ""
    NTx = NestedBundleTxNC(255.U, InnerBundleNC(255.U,255.S,255.U), InnerBundleNC(255.U,255.S,255.U), 255.U)
    randGen.setSeed(999999999.toLong)
    for (_ <- 0 to 9) {
      out1 += NTx.rand.getStringContents
    }

    NTx = NestedBundleTxNC(255.U, InnerBundleNC(255.U,255.S,255.U), InnerBundleNC(255.U,255.S,255.U), 255.U)
    randGen.setSeed(888888888.toLong)
    for (_ <- 0 to 9) {
      out2 += NTx.rand.getStringContents
    }

    (out1 == out2) should be (false)
  }
// KEPT FOR REFERENCE
//  // Problem: Cannot get fields of Vec. Will have to find another way to set  Vec
//  "Vec Test" should "have no error" in {
//    var VTx = TestVecBundle(123.U, Vec(5, UInt(10.W)))
//    println("WHA")
//    for (field <- VTx.getClass.getDeclaredFields) {
//      println(field.getName)
//      if (field.isInstanceOf[Vec[_]]) {
//        for (vecfields <- field.getClass.getFields) {
//          println(vecfields.getName)
//        }
//      }
//    }
//  }
}


class NoChiselDummyRandomTest extends FlatSpec with Matchers {
  implicit val randGen: VerifRandomGenerator = new DummyVerifRandomGenerator
  "Dummy VerifRandomGenerator" should "have no error" in {
    // Testing the dummy random generator
    val NTx = NestedBundleTxNC(100.U, InnerBundleNC(100.U, 100.S, 100.U), InnerBundleNC(100.U, 100.S, 100.U), 100.U)
    for (_ <- 0 to 9) {
      NTx.rand.printContents
    }
  }
}
