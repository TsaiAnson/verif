package verif

import chisel3._
import verif.Randomization._
import utest._

object ConstrainedRandomTest extends TestSuite {
    case class A(w: Int) extends Bundle {
        val x = UInt(w.W)
        val y = UInt(w.W)
    }

    val tests = Tests {
        test("simple constraint") {
            val proto = A(8)
            val randA = proto.rand { bundle =>
                (bundle.x === 3.U)
                //(bundle.x === 3.U) && (bundle.x + bundle.y === 10.U)
            }.right.get
            println(randA.x.litValue())
            println(randA.y.litValue())
        }
    }
}
