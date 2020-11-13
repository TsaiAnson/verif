// Copyright 2020 The Regents of the University of California
// Copyright 2018 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>
// based on the original implementation by Rafael Dutra <rtd@cs.berkeley.edu>


package verif

import maltese.smt
import maltese.smt.solvers.IsSat
import scala.collection.mutable

/** Implements the smt sampler algorithm using Z3
 *
 *  Reference:
 *  Rafael Dutra and Kevin Laeufer and Jonathan Bachrach and Koushik Sen,
 *  "Efficient Sampling of SAT Solutions for Testing,"
 *  in 40th International Conference on Software Engineering (ICSE'18), IEEE, 2018 .
 *
 * */
class SMTSampler private(solver: smt.solvers.Z3SMTLib, support: List[smt.BVSymbol], constraints: List[smt.BVExpr], seed: Long) {
  private val supportBits = support.map(toBits)
  private val random = new scala.util.Random(seed)
  assert(support.nonEmpty, "Cannot work with empty support!")

  def run(): Iterable[Seq[(String, BigInt)]] = {
    // start at a random solution
    val start = getRandomAssignment

    // find closest solution
    val closest = findClosestSolution(start)
    assert(isValid(closest))

    // find neighbors
    val neighbors = computeAllNeighboringSolutions(closest)

    neighbors.map(modelToAssignments)
  }

  private def findClosestSolution(start: List[BigInt]): List[BigInt] = {
    solver.push()
    assignSoft(start)
    val r = solver.check()
    assert(r.isSat)
    val model = readModel()
    solver.pop()
    model
  }

  private def computeAllNeighboringSolutions(start: List[BigInt]): List[List[BigInt]] = {
    // to avoid duplicates
    val seen = mutable.HashSet[BigInt]()
    seen.add(modelToBigInt(start))

    // soft constrain start solution
    solver.push()
    assignSoft(start)

    // all bits that we can flip
    var results = List[List[BigInt]]()
    supportBits.zip(start).foreach { case (bits, value) =>
      bits.foreach { case (expr, ii) =>
        val bitValue = (value >> ii) & 1
        val constraint = smt.BVEqual(expr, smt.BVLiteral(bitValue, 1))
        solver.push()
        solver.assert(smt.BVNot(constraint))
        val r = solver.check()
        if(r.isSat) {
          val model = readModel()
          val bigInt = modelToBigInt(model)
          if(!seen(bigInt)) {
            seen.add(bigInt)
            results = model +: results
          }
        }
        // remove hard constraints
        solver.pop()
      }
    }

    // remove soft constraints
    solver.pop()

    // sanity check
    assert(results.forall(isValid))

    results
  }

  private def modelToAssignments(model: List[BigInt]): List[(String, BigInt)] =
    model.zip(support).map{ case (value, sym) => sym.name -> value }

  private def getRandomAssignment: List[BigInt] = support.map(sym => BigInt(sym.width, random))

  private def readModel(): List[BigInt] = support.map { solver.getValue(_).get }

  // this models the SMTbit approach where every bit is individually constrained
  private def assignSoft(values: List[BigInt]): Unit = {
    supportBits.zip(values).foreach { case (bits, value) =>
      bits.foreach { case (expr, ii) =>
        val bitValue = (value >> ii) & 1
        val constraint = smt.BVEqual(expr, smt.BVLiteral(bitValue, 1))
        solver.softAssert(constraint)
      }
    }
  }

  private def toBits(sym: smt.BVSymbol): List[(smt.BVExpr, Int)] = {
    if(sym.width == 1) { List(sym -> 0) } else {
      (0 until sym.width).map(i => smt.BVSlice(sym, i, i)).zipWithIndex.toList
    }
  }

  // check whether the plugging the model into the formula will result in a true result
  private def isValid(model: List[BigInt]): Boolean = {
    val mapping = support.zip(model).map(t => t._1.name -> t._2).toMap
    val ctx = smt.LocalEvalCtx(mapping)
    val res = constraints.map(c => smt.SMTExprEval.eval(c)(ctx))
    res.forall(_ == 1)
  }

  private val shifts = support.map(_.width).dropRight(1)
  private val singleVar = support.size == 1
  private def modelToBigInt(model: List[BigInt]): BigInt = {
    if(singleVar) { model.head } else {
      model.tail.zip(shifts).foldLeft(model.head) { case (prev, (value, shift)) =>
        (prev << shift) | value
      }
    }
  }
}

object SMTSampler {
  def apply(support: List[smt.BVSymbol], constraints: List[smt.BVExpr], seed: Long = 0): Option[SMTSampler] = {
    val solver = new smt.solvers.Z3SMTLib()
    solver.setLogic(smt.solvers.QF_BV)
    // declare all variables in the support
    support.foreach(s => solver.runCommand(smt.solvers.DeclareFunction(s, Seq())))
    // add all hard constraints
    constraints.foreach(c => solver.assert(c))
    // sanity check to see if the formula is actually satisfiable (if not, there is no use in trying to sample it)
    solver.check() match {
      case IsSat => Some(new SMTSampler(solver, support, constraints, seed))
      case _ => None
    }
  }
}
