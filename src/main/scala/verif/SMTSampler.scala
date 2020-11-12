// Copyright 2020 The Regents of the University of California
// Copyright 2018 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>
// based on the original implementation by Rafael Dutra <rtd@cs.berkeley.edu>


package verif

import maltese.smt
import maltese.smt.solvers.IsSat

/** Implements the smt sampler algorithm using Z3
 *
 *  Reference:
 *  Rafael Dutra and Kevin Laeufer and Jonathan Bachrach and Koushik Sen,
 *  "Efficient Sampling of SAT Solutions for Testing,"
 *  in 40th International Conference on Software Engineering (ICSE'18), IEEE, 2018 .
 *
 * */
class SMTSampler private(solver: smt.solvers.Z3SMTLib, support: Seq[smt.BVSymbol], seed: Long) {
  val width = support.map(_.width).sum
  private val supportBits = support.flatMap(toBits).reverse.zipWithIndex
  assert(supportBits.length == width)
  private val random = new scala.util.Random(seed)


  def run(): Iterable[Seq[(String, BigInt)]] = {
    // start at a random solution
    val start = BigInt(width, random)

    // find closest solution
    val closest = findClosestSolution(start)

    List(modelToAssignments(closest))
  }

  private def modelToAssignments(model: BigInt): Seq[(String, BigInt)] = {
    var res = model
    support.reverse.map { sym =>
      val mask = (BigInt(1) << sym.width) - 1
      val value = res & mask
      res = res >> sym.width
      sym.name -> value
    }.reverse
  }

  private def findClosestSolution(start: BigInt): BigInt = {
    solver.push()
    assignSoft(start)
    val r = solver.check()
    assert(r.isSat)
    val model = readModel()
    solver.pop()
    model
  }

  private def readModel(): BigInt = {
    var res = BigInt(0)
    var resNext = BigInt(0)
    support.reverse.foreach { sym =>
      val value = solver.getValue(sym).get
      res = (resNext | value)
      resNext = (res << sym.width)
    }
    res
  }

  private def assignSoft(bits: BigInt): Unit = {
    supportBits.foreach { case (expr, ii) =>
      val value = (bits >> ii) & 1
      val constraint = smt.BVEqual(expr, smt.BVLiteral(value, 1))
      solver.softAssert(constraint)
    }
  }

  private def toBits(sym: smt.BVSymbol): List[smt.BVExpr] = {
    if(sym.width == 1) { List(sym) } else {
      (0 until sym.width).map(i => smt.BVSlice(sym, i, i)).toList
    }
  }
}

object SMTSampler {
  def apply(support: Seq[smt.BVSymbol], constraints: Seq[smt.BVExpr], seed: Long = 0): Option[SMTSampler] = {
    val solver = new smt.solvers.Z3SMTLib()
    solver.setLogic(smt.solvers.QF_BV)
    // declare all variables in the support
    support.foreach(s => solver.runCommand(smt.solvers.DeclareFunction(s, Seq())))
    // add all hard constraints
    constraints.foreach(c => solver.assert(c))
    // sanity check to see if the formula is actually satisfiable (if not, there is no use in trying to sample it)
    solver.check() match {
      case IsSat => Some(new SMTSampler(solver, support, seed))
      case _ => None
    }
  }
}
