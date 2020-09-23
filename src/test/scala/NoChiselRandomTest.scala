package verif

import org.scalatest._
import verif.Randomization._
import chisel3._
import designs.CAMIO

import scala.collection.mutable.{Map, ListBuffer}

class zeroBundle extends Bundle {
  val empty: UInt = UInt(0.W)
}

case class TestBundleNewRand[T <: Data](gen: T, widthU: Int) extends Bundle {
    val data = gen
    val numb2 = SInt(widthU.W)
    val numb3 = UInt(widthU.W)
    override def cloneType = (TestBundleNewRand(gen, widthU)).asInstanceOf[this.type]
}

case class InnerBundleNC[T <: Data](data: T, numb2: SInt = 0.S(8.W), numb3: UInt = 0.U(8.W)) extends Bundle {
  override def cloneType = InnerBundleNC(data, numb2, numb3).asInstanceOf[this.type]
}

//case class InnerBundleNC[T <: Data](gen: T, widthU: Int) extends Bundle {
//  val data = gen
//  val numb2 = SInt(widthU.W)
//  val numb3 = UInt(widthU.W)
//  override def cloneType = (InnerBundleNC(gen, widthU)).asInstanceOf[this.type]
//}

case class NestedBundleTxNC[T <: Data](data: T, inner1: InnerBundleNC[UInt], inner2: InnerBundleNC[UInt],
                                       numb1: UInt = 0.U) extends Bundle {
  override def cloneType = NestedBundleTxNC(data, inner1.cloneType, inner2.cloneType, numb1).asInstanceOf[this.type]
}


//case class NestedBundleTxNC[T <: Data](gen: T, widthU: Int) extends Bundle {
//  val data = gen
//  val inner1: InnerBundleNC[UInt] = InnerBundleNC(UInt(widthU.W), widthU)
//  val inner2: InnerBundleNC[UInt] = InnerBundleNC(UInt(widthU.W), widthU)
//  val numb1: UInt = UInt(widthU.W)
//  // TODO (low pri): Figure out why cloneType is not automatically inferred
//  override def cloneType = NestedBundleTxNC(gen, widthU).asInstanceOf[this.type]
//}


case class TestBundleTxNC (testB: Bundle) extends Bundle

case class TestVecBundle[T <: Data](data: T, vec1: Vec[T]) extends Bundle

class NoChiselRandomTest extends FlatSpec with Matchers {
  implicit val randGen: VerifRandomGenerator = new ScalaVerifRandomGenerator

  "Basic NoChiselRandomTest" should "have no error" in {
    // Testing Non-nested Structures
    val CTx = CAMIO(8, 8)
    for (_ <- 0 to 2) {
      CTx.rand().printContents
    }

    println("")

    val DTx = DecoupledTX(165.U, 0.U, 1.U)
    for (_ <- 0 to 9) {
      DTx.rand().printContents
    }
  }

  "Nested NoChiselRandomTest" should "have no error" in {
    // Testing Nested Structures
    // Testing with single nested transactions
    val NTx = NestedBundleTxNC(255.U, InnerBundleNC(255.U, 100.S, 255.U), InnerBundleNC(255.U, 100.S, 255.U), 255.U)
    for (_ <- 0 to 9) {
      NTx.rand().printContents
    }
  }

  // Testing empty bundles, bundles with uint of width 0
  "Other Bundle Edge Cases" should "have no error" in {
    val EBTx = TestBundleTxNC(new Bundle{})
    for (_ <- 0 to 9) {
      EBTx.rand().printContents
    }

    val ZBTx = TestBundleTxNC(new zeroBundle)
    for (_ <- 0 to 9) {
      ZBTx.rand().printContents
    }
  }

  "Deterministic Testing" should "have no error" in {
    var out1 = ""
    var out2 = ""

    // Commenting out for now, CAMIO cloneType is not working properly
//    // Testing that two CAMIO's with the same seed should have deterministic rand
//    var CTx = CAMIO(8, 8)
//    randGen.setSeed(1234567890.toLong)
//    for (_ <- 0 to 9) {
//      out1 += CTx.rand().getStringContents
//    }
//
//    CTx = CAMIO(8, 8)
//    randGen.setSeed(1234567890.toLong)
//    for (_ <- 0 to 9) {
//      out2 += CTx.rand().getStringContents
//    }
//
////    print(out1)
////    println("DEBUG")
////    print(out2)
//
//    (out1 == out2) should be (true)
//
//    // Testing that two CAMIO's with the different seed should have deterministic rand
//    out1 = ""
//    out2 = ""
//    CTx = CAMIO(8, 8)
//    randGen.setSeed(1234567890.toLong)
//    for (_ <- 0 to 2) {
//      out1 += CTx.rand().getStringContents
//    }
//
//    CTx = CAMIO(8, 8)
//    randGen.setSeed(987654321.toLong)
//    for (_ <- 0 to 2) {
//      out2 += CTx.rand().getStringContents
//    }
//
//    (out1 == out2) should be (false)

    // Performing the above tests on DecoupledTX to double check
    out1 = ""
    out2 = ""
    var DTx = DecoupledTX(165.U, 0.U, 1.U)
    randGen.setSeed(123123123.toLong)
    for (_ <- 0 to 9) {
      out1 += DTx.rand().getStringContents
    }

    DTx = DecoupledTX(165.U, 0.U, 1.U)
    randGen.setSeed(123123123.toLong)
    for (_ <- 0 to 9) {
      out2 += DTx.rand().getStringContents
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
      out1 += DTx.rand().getStringContents
    }

    DTx = DecoupledTX(165.U, 0.U, 1.U)
    randGen.setSeed(222222222.toLong)
    for (_ <- 0 to 9) {
      out2 += DTx.rand().getStringContents
    }

    (out1 == out2) should be (false)

    // Performing the above tests on the Nested Transactions to triple check
    out1 = ""
    out2 = ""
//    var NTx = NestedBundleTxNC(UInt(8.W), 8)
    var NTx = NestedBundleTxNC(255.U, InnerBundleNC(255.U, 100.S, 255.U), InnerBundleNC(255.U, 100.S, 255.U), 255.U)
    randGen.setSeed(676767.toLong)
    for (_ <- 0 to 9) {
      out1 += NTx.rand().getStringContents
    }

    NTx = NestedBundleTxNC(255.U, InnerBundleNC(255.U, 100.S, 255.U), InnerBundleNC(255.U, 100.S, 255.U), 255.U)
    randGen.setSeed(676767.toLong)
    for (_ <- 0 to 9) {
      out2 += NTx.rand().getStringContents
    }

//    print(out1)
//    println("DEBUG")
//    print(out2)

    (out1 == out2) should be (true)

    out1 = ""
    out2 = ""
    NTx = NestedBundleTxNC(255.U, InnerBundleNC(255.U, 100.S, 255.U), InnerBundleNC(255.U, 100.S, 255.U), 255.U)
    randGen.setSeed(999999999.toLong)
    for (_ <- 0 to 9) {
      out1 += NTx.rand().getStringContents
    }

    NTx = NestedBundleTxNC(255.U, InnerBundleNC(255.U, 100.S, 255.U), InnerBundleNC(255.U, 100.S, 255.U), 255.U)
    randGen.setSeed(888888888.toLong)
    for (_ <- 0 to 9) {
      out2 += NTx.rand().getStringContents
    }

    (out1 == out2) should be (false)
  }

  // Testing that rand() returns a new bundle
  "Independent Bundle" should "have no error" in {
    val NTx_proto = NestedBundleTxNC(255.U, InnerBundleNC(255.U, 100.S, 255.U), InnerBundleNC(255.U, 100.S, 255.U), 255.U)

    val NTx_temp = NTx_proto.rand()
    val NTx_temp1 = NTx_proto.rand()

    // Checking if they are different objects
    (NTx_temp == NTx_temp1) should be (false)

    // Hardcoded for this specific case, but the inner bundles should also be different
    (NTx_temp.inner1 == NTx_temp1.inner1) should be (false)
    (NTx_temp.inner2 == NTx_temp1.inner2) should be (false)
  }

  "Randomization Constraints" should "have no error" in {
    // Currently declaring the constraints structure outside of the VerifBundle as a work-around
    var constraints: Map[String, ListBuffer[Data => Bool]] = Map[String, ListBuffer[Data => Bool]]()

    var NTx_proto  = NestedBundleTxNC(255.U, InnerBundleNC(255.U, 100.S, 255.U), InnerBundleNC(255.U, 100.S, 255.U), 255.U)

    // Adding constraints to field named "data" (all UInts in this example)
    constraints += ("data" -> new ListBuffer[Data => Bool])
    // Had to convert to scala types as chisel type operation can only happen within user module
    constraints("data") += {v: Data => (v.litValue().toInt > 30).B}
    constraints("data") += {v: Data => (v.litValue().toInt < 100).B}

    // Adding another field
    constraints += ("numb1" -> new ListBuffer[Data => Bool])
    constraints("numb1") += {v: Data => (v.litValue().toInt > 10 && v.litValue().toInt < 50).B}

    NTx_proto.rand(constraints).printContents
    NTx_proto.rand(constraints).printContents
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


//class NoChiselDummyRandomTest extends FlatSpec with Matchers {
//  implicit val randGen: VerifRandomGenerator = new DummyVerifRandomGenerator
//  "Dummy VerifRandomGenerator" should "have no error" in {
//    // Testing the dummy random generator
//    val NTx = NestedBundleTxNC(255.U, InnerBundleNC(255.U, 100.S, 255.U), InnerBundleNC(255.U, 100.S, 255.U), 255.U)
//    for (_ <- 0 to 9) {
//      NTx.rand().printContents
//    }
//  }
//}
//
class NoChiselNewRandTest extends FlatSpec with Matchers {
  "Firrtl stuff" should "have no error" in {
    val B = TestBundleNewRand(UInt(8.W), 8)
    B.randNew({b: TestBundleNewRand[UInt] => b.numb3 === 6.U})
  }
}
