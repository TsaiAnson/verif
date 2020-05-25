package verif

import chisel3._
import chisel3.util._
import chiseltest._

import scala.collection.mutable.{MutableList, Queue}

trait Transaction extends Bundle {
  // Will define later when working with constraint solver
  def rand(): Int = 0


}

// // Playing around with scala
// trait Transaction {
//   def rand(): Unit = {
//    // Gets all fields and sets them to a random int
//    val r = scala.util.Random
//    for (field <- this.getClass.getDeclaredFields) {
//      field.setAccessible(true)
//      // println(field.getName)
//      // println(field.get(this))
//      field.set(this, r.nextInt())
//    }
//   }
// }

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


