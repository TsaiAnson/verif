package verif

import scala.util.Random
import java.lang.reflect.Field
import java.nio.charset.StandardCharsets

import chisel3._
import chisel3.experimental.DataMirror
import chisel3.stage.ChiselCircuitAnnotation
import chisel3.stage.phases.Elaborate
import firrtl.AnnotationSeq
import firrtl.backends.experimental.smt.EmittedSMTModelAnnotation
import firrtl.stage.{FirrtlSourceAnnotation, FirrtlStage}
import firrtl.options.TargetDirAnnotation

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Map, Queue}
import scala.io.Source
import scala.sys.process.{BasicIO, Process, ProcessLogger}

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

    sealed trait RandomizationError
    case class Unsat() extends RandomizationError
    case class Timeout() extends RandomizationError

    def rand(constraint: T => Bool): Either[RandomizationError, T] = {
      class RandomBundleWrapper extends RawModule {
        val clock = IO(Input(Clock()))
        val b = IO(Input(bundle.cloneType))
        val c = constraint(b)
        dontTouch(c)
        dontTouch(b)
        withClock(clock) {
          chisel3.experimental.verification.assert(c)
        }
      }

      val generatorAnnotation = AnnotationSeq(Seq(chisel3.stage.ChiselGeneratorAnnotation(() => new RandomBundleWrapper)))
      val chiselAnnosOut = (new Elaborate).transform(generatorAnnotation)
      val chiselCircuit = chiselAnnosOut.collect { case x: ChiselCircuitAnnotation => x}.head.circuit
      val chiselModule = chiselCircuit.components.find(_.name == chiselCircuit.name).get.id.asInstanceOf[RandomBundleWrapper]
      val portNames = DataMirror.fullModulePorts(chiselModule).drop(1) // drop clock
      println(portNames)

      // TODO: avoid double elaboration
      val fir = (new chisel3.stage.ChiselStage).emitFirrtl(
        new RandomBundleWrapper, Array("--target-dir", "./rand"), List.empty)

      // TODO: find a way to invoke FIRRTL without the CLI frontend
      val fir_annos = List(
        FirrtlSourceAnnotation(fir),
        TargetDirAnnotation("./rand")
        // SMTLibEmitter is private, only available in firrtl package
        //RunFirrtlTransformAnnotation(new SMTLibEmitter),
        //EmitCircuitAnnotation(classOf[SMTLibEmitter])
      )
      val fir_args = Array(
        "-ll",
        "error",
        "-E",
        "experimental-smt2"
      )
      val fir_resp = (new FirrtlStage).execute(fir_args, fir_annos)

      // TODO: Ugly, what's the right way to fetch a specific subtype of Annotation
      val smtModelAnno = fir_resp.toSeq.find {
        case a: EmittedSMTModelAnnotation => true
        case _ => false
      }.get.asInstanceOf[EmittedSMTModelAnnotation]

      // TODO: don't hardcode the assertion,
      // This won't work with if there are namespace clashes with k
      val smtString = smtModelAnno.src +
        "(declare-fun k () (RandomBundleWrapper_s))\n" +
        "(assert (= (RandomBundleWrapper_a k) true))\n" +
        "(check-sat)\n" +
        "(get-model)"

      var z3out = mutable.MutableList.empty[String]
      val z3p = Process(Seq("z3", "-in"))
      val io =
        BasicIO.standard(true)
          .withInput { w =>
            w.write(smtString.getBytes(StandardCharsets.UTF_8))
            w.close()
          }
          .withOutput { i =>
            Source.fromInputStream(i).getLines().foreach {
              s: String => z3out += s
            }
            i.close()
          }

      // TODO: implement timeout
      val ret = z3p.run(io).exitValue()
      // TODO: check ret

      val z3outSplit = z3out.toIterator
      val checkSat = z3outSplit.next()
      if (checkSat == "unsat") {
        return Left(Unsat())
      }
      assert(checkSat == "sat") // TODO: could be indeterminate


      // TODO: parse SAT model using library
      assert(z3outSplit.next().trim() == "(model")
      val model = mutable.Map[String, Int]() // TODO: can't handle hierchical bundles
      for (define <- z3outSplit.grouped(2).toList) {
        if (define.length == 2) {
          val variable = define(0).trim()
          val charsUntilSpace = variable.indexOf(' ')
          val cutDefineFun = variable.substring(charsUntilSpace+1)
          val nextSpace = cutDefineFun.indexOf(' ')
          val variableName = cutDefineFun.substring(0, nextSpace)
          assert(!variableName.contains(' '))
          if (variableName.startsWith("b")) { // TODO: hardcoded bundle pointer
            val bundlePath = variableName.stripSuffix("_f")
            //val bundleFieldName = bundlePath(1)
            val data = define(1).trim().stripPrefix("#x").stripSuffix(")").toInt // TODO: can't handle larger numbers
            model += (bundlePath -> data)
          }
        }
      }

      import chisel3.experimental.BundleLiterals._
      println(model)
      // TODO: ugly, would like a way to construct a Bundle from a Map without reflection (via macro?)
      val binding = mutable.Map[Data, Data]()
      for ((portName, portBind) <- portNames) {
        portBind match {
          case u: UInt =>
            binding += (u -> model(portName).U(u.getWidth))
          case _ =>
        }
      }

      // getElements doesn't return the correct field binding
     /*
      for (field <- bundle.getElements) {
        field match {
          case u: UInt =>
            assert(u.widthKnown)
            assert(u.getWidth != 0)
            println(u.computeName(None, None))
            //binding += (u -> model())
        }
        println(field)
      }
     */

      // Using Scala reflection fails as well
      /*
      for (field <- bundle.getClass.getDeclaredFields) {
        field.setAccessible(true)
        field.get(bundle).asInstanceOf[Any] match {
          case _: Bool =>
            //field.set(randomBundle, model(field.getName).B)
          case u: UInt =>
            assert(u.widthKnown)
            assert(u.getWidth != 0)
            //u := model(List(field.getName)).U(u.getWidth)
            println(field.getName, u.getWidth, model(field.getName))
            binding += (u -> model(field.getName).U(u.getWidth))
            //field.set(randomBundle, model(field.getName).U(u.getWidth))
          case _: Data =>
            println(s"[VERIF] WARNING: Skipping randomization of unknown chisel type,value: " +
              s"(${field.getName}:${field.getType},${field.get(bundle)})")
          case _: Any =>
        }
      }
       */

      println(binding)
      val bindingAsFunctions = binding.toSeq.map { bind =>
        new Function1[T, (Data, Data)] {
          def apply(t: T): (Data, Data) = t.getElements(0) -> bind._1
        }
      }
      //val randomBundle = chiselModule.b.cloneType.Lit(bindingAsFunctions:_*)
      val randomBundle = chiselModule.b.cloneType.Lit(_.getElements(0) -> 8.U, _.getElements(1) -> 10.U)
      println(randomBundle)
      Right(randomBundle)
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


