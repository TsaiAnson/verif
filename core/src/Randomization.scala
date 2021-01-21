package verif

import chisel3._
import chisel3.experimental.DataMirror
import chisel3.experimental.BundleLiterals._
import maltese.passes.Inline
import maltese.smt
import maltese.mc.IsConstraint

sealed trait RandomizationError
case class Unsat() extends RandomizationError
case class Timeout() extends RandomizationError

package object Randomization {
  implicit class VerifBundle[T <: Bundle](bundle: T) extends Bundle {
    def rand(constraint: T => Bool): Either[RandomizationError, T] = {
      class RandomBundleWrapper extends RawModule {
        val clock = IO(Input(Clock()))
        val b = IO(Input(bundle.cloneType))
        val c = constraint(b)
        dontTouch(c)
        dontTouch(b)
        withClock(clock) {
          chisel3.experimental.verification.assume(c)
        }
      }

      val (state, module) = ChiselCompiler.elaborate(() => new RandomBundleWrapper)
      val portNames = DataMirror.fullModulePorts(module).drop(2) // drop clock and top-level 'b' IO

      // turn firrtl into a transition system
      val (sys, _) = FirrtlToFormal(state.circuit, state.annotations)
      // inline all signals
      val inlinedSys = Inline.run(sys)

      val support = inlinedSys.inputs
      val constraints = inlinedSys.signals.filter(_.lbl == IsConstraint).map(_.e.asInstanceOf[smt.BVExpr])
      SMTSampler(support, constraints) match {
        case Some(sampler) =>
          val samples = sampler.run()
          // TODO: use more than one sample
          val model = samples.head.toMap

          val modelBinding = portNames.map(_._1).zipWithIndex.map { case (name, index) =>
            new Function1[T, (Data, Data)] {
              def apply(t: T): (Data, Data) = t.getElements(index) -> model(name).U
            }
          }
          val randomBundle = module.b.cloneType.Lit(modelBinding.toSeq: _*)
          Right(randomBundle)
        case None => Left(Unsat())
      }
    }
  }
}
