package verif

import scala.util.Random
import chisel3._
import chisel3.util._
import chiseltest._
import java.lang.reflect.Field

import scala.collection.mutable
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

package object Randomization {
  implicit class VerifBundle(bundle: Bundle) extends Bundle {
    // Caching no longer seems to work within implicit class
    val declaredFields: Map[Class[_], Array[Field]] = Map[Class[_],Array[Field]]()

    // Want to have constraints structure saved with each bundle, not currently working (similar problem to above)
    // Would want to implement caching, so each field would be assigned a list of constraints
    // For example: constraints: Map[String, Array[Bundle => Bool]]
    // Currently just integers to test if the structures would be saved (they're not)
    val constraints: MutableList[Int] = new MutableList[Int]

    def addConstraint(test: Int): Unit = {
      // Currently just integer list for sanity checks
      constraints += test
      println(constraints)
      // Would add to cache-map here
    }

    def printConstraint: Unit = {
      println(constraints)
    }

//    def cloneBundle: Bundle = {
//      val newclone = bundle.getClass.newInstance()
//      newclone
//    }

    // Using single constraint as an example. Planned to have separate constraint functions (see above)
    // Proof of Concept: UInt range
    def rand (constraint: UInt => Bool = {v : Data => true.B}) (implicit randgen: VerifRandomGenerator): Bundle = {
      rand_helper(bundle, constraint)
      bundle
    }

    // Helper function for rand
    def rand_helper(b : Bundle, constraint: UInt => Bool) (implicit randgen: VerifRandomGenerator): Unit = {
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
            rand_helper(bundle, constraint)
          case uval: UInt =>
            if (uval.getWidth != 0) {
              var newval = randgen.getNextUInt(uval.getWidth)
              while (!satisfy(newval, constraint).litToBoolean) {
                newval = randgen.getNextUInt(uval.getWidth)
              }
              field.set(b, newval)
            }
          case sval: SInt =>
            if (sval.getWidth != 0) {
              // Handling for negative numbers (2's complement)
              field.set(b, randgen.getNextSInt(sval.getWidth))
            }
          case _: Data =>
            println(s"[VERIF] WARNING: Skipping randomization of unknown chisel type,value: " +
              s"(${field.getName}:${field.getType},${field.get(b)})")
          case _: Any =>
          // Do nothing
        }
      }
    }

    def satisfy(value: UInt, constraint: UInt => Bool): Bool = {
      constraint(value)
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
              // Hardcoded for single-nested bundles. Just for proof-of-concept
              if (field1.get(bundle).isInstanceOf[Bundle]) {
                val innerBundle = field1.get(bundle)
                result += s"Bundle ${field1.getName} {"
                for (field2 <- innerBundle.getClass.getDeclaredFields) {
                  field2.setAccessible(true)
                  result += s"(${field2.getName}, ${field2.get(innerBundle)}) "
                }
                result += "} "
              } else {
                result += s"(${field1.getName}, ${field1.get(bundle)}) "
              }
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
            result += s"(${field.getName}:${field.getType}, ${field.get(this)}) "
        }
      }
      result += "\n"
      result
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


