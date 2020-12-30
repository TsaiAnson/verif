package verif

import chisel3._
import chisel3.fromIntToLiteral
import verif.Randomization._
import TestBundles._
import org.scalatest.flatspec.AnyFlatSpec

class ConstrainedRandomTest extends AnyFlatSpec {
    behavior of "constrained random"
    it should "constrain with simple adds and equalities" in {
        val proto = A(8)
        val randA = proto.rand { bundle =>
            (bundle.x === 3.U) && (bundle.x + bundle.y === 10.U)
        }.right.get
        assert(randA.x.litValue() == 3)
        assert(randA.y.litValue() == 7)
    }
    it should "constrain with multiplies" in {
        val proto = A(8)
        val b = proto.rand { bundle =>
            (bundle.x * bundle.y === 17.U)
        }.right.get
        assert(b.x.litValue() * b.y.litValue() == 17)
    }
    it should "catch unsat constraints" in {
        val proto = A(8)
        val b = proto.rand { bundle =>
            (bundle.x === bundle.y) &&
              (bundle.x =/= bundle.y)
        }
        assert(b.isLeft)
        assert(b.left.get.isInstanceOf[Unsat])
    }
    it should "work with Muxes" in {
        val k = K().rand { b =>
            Mux(b.b.asBool(), b.x > b.y, b.x < b.y) &&
              (b.b === 0.U) && (b.x +& b.y === 5.U)
        }
        println(k)
        assert(k.isRight)
        val kLit = k.right.get
        assert(kLit.x.litValue() < kLit.y.litValue())
        assert(kLit.b.litValue() == 0)
        assert(kLit.x.litValue() + kLit.y.litValue() == 5)
    }
    ignore should "work with Bundles with Vecs" in {
        case class K() extends Bundle {
            val x = Vec(4, UInt(8.W))
        }
        val k = K().rand { bundle =>
            bundle.x.map(_ === 7.U).reduce(_ & _)
        }
        assert(k.isRight)
        k.right.get.x.map(_.litValue()).foreach { elem => assert(elem == 7) }
    }
}