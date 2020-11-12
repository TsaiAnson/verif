// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package firrtl.backends.experimental.smt

import firrtl.ir

object FirrtlToSMT {
  def toWidth(tpe: ir.Type): Int = FirrtlExpressionSemantics.getWidth(tpe)

}
