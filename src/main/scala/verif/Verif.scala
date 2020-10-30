package verif

import scala.util.Random
import chisel3._
import chisel3.util._
import chiseltest._
import java.lang.reflect.Field

import chisel3.stage.ChiselGeneratorAnnotation

import scala.collection.mutable.{ListBuffer, Map, Queue}

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
  implicit class VerifBundle[T <: Bundle](bundle: T) extends Bundle {
    // Caching no longer seems to work within implicit class, seems like the variable is cleared each time
    val declaredFields: Map[Class[_], Array[Field]] = Map[Class[_],Array[Field]]()

    // Want to have constraints structure saved with each bundle, not currently working (similar problem to above)
    // Functions defined here for reference
    var constraints: Map[String, ListBuffer[Data => Bool]] = Map[String, ListBuffer[Data => Bool]]()

    // Ignore
    def addConstraint(fieldName: String, constraint: Data => Bool): Unit = {
      constraints(fieldName) += constraint
    }

//    def cloneBundle: Bundle = {
//      val newclone = bundle.getClass.newInstance()
//      newclone
//    }

    def randNew (constraint: T => Bool): T = {
      class dummy extends MultiIOModule {
        val b = IO(bundle.cloneType)

        constraint(b)
        dontTouch(b)
      }

      val annos = List(
        ChiselGeneratorAnnotation(() => new dummy)
      )
      (new chisel3.stage.ChiselStage).execute(Array.empty, annos)

      bundle.cloneType
    }

    // Pass in constraint map. A listbuffer of cosntraints is mapped to field names (text). Currently, only supports
    // independent constraints (no dependencies). Also, only works with non-clashing field names. Currently proof-of-
    // concept.
    def rand (constraint: Map[String, ListBuffer[Data => Bool]] = Map("" -> new ListBuffer[Data => Bool])) (implicit randgen: VerifRandomGenerator): T = {
      rand_helper(bundle, constraint)
      bundle.cloneType
    }

    // Helper function for rand
    def rand_helper(b : Bundle, constraints: Map[String, ListBuffer[Data => Bool]] = Map("" -> new ListBuffer[Data => Bool])) (implicit randgen: VerifRandomGenerator): Unit = {
      // Caching
      if (!declaredFields.contains(b.getClass)) {
        declaredFields += (b.getClass -> b.getClass.getDeclaredFields)
      }

      // Randomize individual fields. Currently, only the UInt and SInt fields use the constraint map.
      for (field <- declaredFields(b.getClass)) {
        field.setAccessible(true)

        field.get(b).asInstanceOf[Any] match {
          case _: Bool =>
            field.set(b, randgen.getNextBool)
          case bundle: Bundle =>
            rand_helper(bundle, constraints)
          case uval: UInt =>
            if (uval.getWidth != 0) {
              var newval = randgen.getNextUInt(uval.getWidth)
              if (constraints.contains(field.getName)) {
                while (!satisfy(newval, constraints(field.getName)).litToBoolean) {
                  newval = randgen.getNextUInt(uval.getWidth)
                }
              }
              field.set(b, newval)
            }
          case sval: SInt =>
            if (sval.getWidth != 0) {
              var newval = randgen.getNextSInt(sval.getWidth)
              if (constraints.contains(field.getName)) {
                while (!satisfy(newval, constraints(field.getName)).litToBoolean) {
                  newval = randgen.getNextSInt(sval.getWidth)
                }
              }
              field.set(b, newval)
            }
          case _: Data =>
            println(s"[VERIF] WARNING: Skipping randomization of unknown chisel type,value: " +
              s"(${field.getName}:${field.getType},${field.get(b)})")
          case _: Any =>
          // Do nothing
        }
      }
    }

    def satisfy(value: Data, constraints: ListBuffer[Data => Bool]): Bool = {
      var sat = true
      for (constraint <- constraints) {
        sat &= constraint(value).litToBoolean
      }
      sat.B
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
  def checkOutput[T](dutOutput : Array[_ <: T], dutFn : T => Any,
                     swOutput : Array[_ <: T], swFn : T => Any) : Boolean = {
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


