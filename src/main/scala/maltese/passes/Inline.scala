// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>


package maltese.passes

import maltese.mc._
import maltese.smt._

import scala.collection.mutable


object Inline {
  def run(sys: TransitionSystem): TransitionSystem = {
    val inlineExpr = mutable.HashMap[String, SMTExpr]()
    val signals = sys.signals.map { signal =>
      val inlinedE = replaceSymbols(inlineExpr.get)(signal.e)
      inlineExpr(signal.name) = inlinedE
      signal.copy(e = inlinedE)
    }
    sys.copy(signals = signals)
  }

  private def replaceSymbols(inlineExpr: String => Option[SMTExpr])(e: SMTExpr): SMTExpr = e match {
    case s @ BVSymbol(name, _) => inlineExpr(name).getOrElse(s)
    case s @ ArraySymbol(name, _, _) => inlineExpr(name).getOrElse(s)
    case other => other.mapExpr(replaceSymbols(inlineExpr))
  }
}