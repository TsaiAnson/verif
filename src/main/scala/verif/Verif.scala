package verif

import scala.util.Random
import chisel3._
import chisel3.util._
import chiseltest._

import scala.collection.mutable.{MutableList, Queue}

trait Transaction extends Bundle {
  private val r = Random
  r.setSeed(1234567890)

  // Set seed for randomization
  def setSeed(seed : Long): Unit = {
    r.setSeed(seed)
  }

  // Currently randomizes fields using no constraints
  def rand: Unit = {
    rand_helper(this)
  }

  // Helper function for rand
  def rand_helper(b : Bundle): Unit = {
    for (field <- b.getClass.getDeclaredFields) {
      field.setAccessible(true)

      field.get(b).asInstanceOf[Any] match {
        case _: Bool =>
          field.set(b, r.nextBoolean().B)
        case bundle: Bundle =>
          rand_helper(bundle)
        case uval: UInt =>
          if (uval.getWidth != 0) {
            field.set(b, (r.nextInt().abs % Math.pow(2, uval.getWidth).toInt).U(uval.getWidth.W))
          }
        case sval: SInt =>
          if (sval.getWidth != 0) {
            // Handling for negative numbers (2's complement)
            field.set(b, ((r.nextInt().abs % Math.pow(2, sval.getWidth).toInt) - Math.pow(2, sval.getWidth - 1).toInt).S(sval.getWidth.W))
          }
        case _: Any =>
//          println(s"Skipping randomization of non-chisel type: (${field.getName},${field.get(b)})")
      }
    }
  }

  // Temporary function to print for debug
  // Print contents of transaction
  // Can only handle single-nested bundles for now
  def listContents: Unit = {
    for (field <- this.getClass.getDeclaredFields) {
      field.setAccessible(true)
      field.get(this).asInstanceOf[Any] match {
        case bundle: Bundle =>
          print(s"Bundle ${field.getName} {")
          for (field1 <- bundle.getClass.getDeclaredFields) {
            field1.setAccessible(true)
            print(s"(${field1.getName}, ${field1.get(bundle)}) ")
          }
          print("} ")
        case _: Any =>
          print(field.getName, field.get(this)); print(" ")
      }
    }
    println("")
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


