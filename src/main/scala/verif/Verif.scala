package verif

import scala.util.Random
import chisel3._
import chisel3.util._
import chiseltest._
import java.lang.reflect.Field

import scala.collection.mutable.{Map, MutableList, Queue}

trait VerifRandomGenerator {
  def setSeed(seed: Long): Unit
  def getNextBool: Bool
  def getNextUInt(width: Int): UInt
  def getNextSInt(width: Int): SInt
  // Can add more types
}

class ScalaVerifRandomGenerator extends VerifRandomGenerator {
  private val r = Random
  r.setSeed(1234567890.toLong)

  // Set seed for randomization
  def setSeed(seed : Long): Unit = {
    r.setSeed(seed)
  }

  def getNextBool: Bool = {
    r.nextBoolean().B
  }

  def getNextUInt(width: Int): UInt = {
    (r.nextInt().abs % Math.pow(2, width).toInt).U(width.W)
  }

  def getNextSInt(width: Int): SInt = {
    ((r.nextInt().abs % Math.pow(2, width).toInt) - Math.pow(2, width - 1).toInt).S(width.W)
  }

}

class DummyVerifRandomGenerator extends VerifRandomGenerator {
  var current_val = -1
  def setSeed(seed: Long): Unit = {

  }

  def getNextBool: Bool = {
    current_val += 1
    (current_val % 2).B
  }

  def getNextUInt(width: Int): UInt = {
    current_val += 1
    (current_val % Math.pow(2, width).toInt).U(width.W)
  }

  def getNextSInt(width: Int): SInt = {
    current_val += 1
    ((current_val.abs % Math.pow(2, width).toInt) - Math.pow(2, width - 1).toInt).S(width.W)
  }
}

// Can define more VerifRandomGenerators Here

class Transaction(implicit randgen: VerifRandomGenerator) extends Bundle {

  private val declaredFields = Map[Class[_],Array[Field]]()

  // Currently randomizes fields using no constraints
  def rand: Transaction = {
    rand_helper(this)
    this
  }

  // Helper function for rand
  def rand_helper(b : Bundle): Unit = {
    // Caching
    if (!declaredFields.contains(b.getClass)) {
      declaredFields += (b.getClass -> b.getClass.getDeclaredFields)
    }

    for (field <- declaredFields(b.getClass)) {
      field.setAccessible(true)

      field.get(b).asInstanceOf[Any] match {
        case _: Bool =>
          field.set(b, randgen.getNextBool)
        case bundle: Bundle =>
          rand_helper(bundle)
        case uval: UInt =>
          if (uval.getWidth != 0) {
            field.set(b, randgen.getNextUInt(uval.getWidth))
          }
        case sval: SInt =>
          if (sval.getWidth != 0) {
            // Handling for negative numbers (2's complement)
            field.set(b, randgen.getNextSInt(sval.getWidth))
          }
        case _: Data =>
          println(s"[VERIF] WARNING: Skipping randomization of unknown chisel type,value: " +
            s"(${field.getName},${field.get(b)})")
        case _: Any =>
          // Do nothing
      }
    }
  }

  // Temporary function to print for debug
  // Print contents of transaction
  // Can only handle single-nested bundles for now
  def printContents: Unit = {
    print(this.getStringContents)
  }

  def getStringContents: String = {
    var result = ""
    for (field <- this.getClass.getDeclaredFields) {
      field.setAccessible(true)
      field.get(this).asInstanceOf[Any] match {
        case bundle: Bundle =>
          result += s"Bundle ${field.getName} {"
          for (field1 <- bundle.getClass.getDeclaredFields) {
            field1.setAccessible(true)
            result += s"(${field1.getName}, ${field1.get(bundle)}) "
          }
          result += "} "
        case map: Map[Class[_],Array[Field]] =>
          // Listing out Map contents
          result += s"Map ${field.getName} {"
          for (key <- map.keys) {
            result += s"(key: "
            for (field1 <- map(key)) {
              field1.setAccessible(true)
              result += s"${field1.getName} "
            }
            result += ") "
          }
          result += "} "
        case _: VerifRandomGenerator =>
          result += s"RandomGen(${field.getName})"
        case _: Any =>
          result += s"(${field.getName}, ${field.get(this)}) "
      }
    }
    result += "\n"
    result
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


