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

