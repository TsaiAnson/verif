package verif

import org.scalatest.flatspec.AnyFlatSpec
import verif.TestBundles.K
import chisel3.experimental.BundleLiterals._
import chisel3._

class TransactionTest extends AnyFlatSpec {
  behavior of "Transaction mixin"
  it should "check literal equality for a flat Bundle" in {
    val a = K().Lit(_.x -> 1.U, _.y -> 2.U, _.b -> 0.U)
    val b = K().Lit(_.x -> 1.U, _.y -> 2.U, _.b -> 0.U)
    val c = K().Lit(_.x -> 1.U, _.y -> 3.U, _.b -> 0.U)
    val d = K().Lit(_.x -> 1.U, _.y -> 2.U, _.b -> 1.U)
    assert(a == b)
    assert(a != c)
    assert(b != c)
    assert(a != d)
    assert(b != d)
    assert(c != d)
  }
}

