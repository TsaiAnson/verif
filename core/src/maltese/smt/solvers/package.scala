// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.smt

package object solvers {
  val QF_BV = SMTFeature.QuantifierFree + SMTFeature.BitVector
  val QF_ABV = SMTFeature.QuantifierFree + SMTFeature.Array + SMTFeature.BitVector
}
