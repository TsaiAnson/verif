package verif

import chisel3._
import chisel3.util._
import chiseltest._

import scala.collection.mutable.{MutableList, Queue}

trait Transaction extends Bundle {
  // Currently randomizes fields using no constraints
  def rand(): Unit = {
    rand_helper(this)
  }

  // Helper function for rand
  def rand_helper(b : Bundle): Unit = {
    val r = scala.util.Random
    for (field <- b.getClass.getDeclaredFields) {
      field.setAccessible(true)

      field.get(b).asInstanceOf[Any] match {
        case _: Bool =>
          field.set(b, r.nextBoolean().B)
        case bundle: Bundle =>
          rand_helper(bundle)
        case c: UInt =>
          field.set(b, ((r.nextInt().abs) % Math.pow(2, c.getWidth).toInt).U(c.getWidth.W))
        case _: Any =>
          println(s"Skipping randomization of non-chisel type: (${field.getName},${field.get(b)})")
      }
    }
  }
}

package object outputChecker {
  def checkOutput[T](dutOutput : Array[T], dutFn : T => Any,
                     swOutput : Array[T], swFn : T => Any) : Boolean = {
    if (dutOutput.map(t => dutFn(t)).sameElements(swOutput.map(t => swFn(t)))) {
      println("***** PASSED *****")
      val outputsize = dutOutput.length
      println(s"All $outputsize transactions were matched.")
      true
    } else {
      println("***** FAILED *****")
      // Will need a better way of printing differences
      println("========DUT========")
      for (t <- dutOutput) {
        println(dutFn(t))
      }
      println("========GOLDEN MODEL========")
      for (t <- swOutput) {
        println(swFn(t))
      }
      false
    }
  }
}


