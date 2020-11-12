// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese

package object smt {
  type Solver = solvers.Solver
  type Logic = solvers.Solver.Logic
  val SMTFeature = solvers.SMTFeature
}
