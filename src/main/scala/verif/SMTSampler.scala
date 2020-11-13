// Copyright 2020 The Regents of the University of California
// Copyright 2018 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>
// based on the original implementation by Rafael Dutra <rtd@cs.berkeley.edu>


package verif

import maltese.smt
import maltese.smt.solvers.IsSat
import scala.collection.mutable

case class SMTSamplerOptions(alphaMin: Double, maxCombine: Int, maxSamples: Int)

/** Implements the smt sampler algorithm using Z3
 *
 *  Reference:
 *  Rafael Dutra and Kevin Laeufer and Jonathan Bachrach and Koushik Sen,
 *  "Efficient Sampling of SAT Solutions for Testing,"
 *  in 40th International Conference on Software Engineering (ICSE'18), IEEE, 2018 .
 *
 * */
class SMTSampler private(
  solver: smt.solvers.Z3SMTLib, opt: SMTSamplerOptions,
  support: List[smt.BVSymbol], constraints: List[smt.BVExpr],
  random: scala.util.Random) {
  private type Model = List[BigInt]
  private val supportBits = support.map(toBits)
  assert(support.nonEmpty, "Cannot work with empty support!")

  def run(): Iterable[Seq[(String, BigInt)]] = {
    // start at a random solution
    val start = getRandomAssignment
    val res = epoch(start, opt.maxSamples)

    // exit solver
    solver.close()

    res.take(opt.maxSamples).map(modelToAssignments)
  }

  private def epoch(randomAssignment: Model, maxSamples: Int): List[Model] = {
    // find closest solution
    val start = findClosestSolution(randomAssignment)
    assert(isValid(start))

    // to avoid duplicates
    val seen = mutable.HashSet[BigInt]()
    seen.add(modelToBigInt(start))

    // find neighbors
    val neighbors = computeAllNeighboringSolutions(start, seen)

    // check if we already have enough
    if(neighbors.size + 1 >= maxSamples) {
     start +: neighbors
    } else {
      val (_, _, solutions) =
        (0 to opt.maxCombine).foldLeft((1.0, neighbors, neighbors)) {
          case ((prevAlpha, last, all), k) =>
            if (prevAlpha < opt.alphaMin || all.size + 1 >= maxSamples) {
              (0.0, last, all)
            } else {
              val remaining = maxSamples - all.size - 1
              val (newSolutions, newAlpha) = combine(start, last, neighbors, seen, remaining)
              (newAlpha, newSolutions, all ++ newSolutions)
            }
        }
      start +: solutions
    }
  }

  private def findClosestSolution(start: Model): Model = {
    solver.push()
    assignSoft(start)
    val r = solver.check()
    assert(r.isSat)
    val model = readModel()
    solver.pop()
    model
  }

  private def computeAllNeighboringSolutions(start: Model, seen: mutable.HashSet[BigInt]): List[Model] = {
    // soft constrain start solution
    solver.push()
    assignSoft(start)

    // all bits that we can flip
    var results = List[Model]()
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

  private def combine(start: Model, last: List[Model], neighbors: List[Model], seen: mutable.HashSet[BigInt], maxSamples: Int):
  (List[Model], Double) = {
    var newCount = 0
    var validCount = 0
    val solutions = neighbors.flatMap { a =>
      last.flatMap { b =>
        if (validCount < maxSamples) {
          // combine mutations relative to the starting solution
          val n = phi(start, a, b)

          // check if we have already seen this solution
          val i = modelToBigInt(n)
          if (!seen(i)) {
            seen.add(i)
            newCount += 1

            // check to see if the solution is actually a valid solution
            if (isValid(n)) {
              validCount += 1
              Some(n)
            } else { None }
          } else { None }
        } else { None }
      }
    }
    val alpha = validCount.toDouble / newCount.toDouble
    (solutions, alpha)
  }

  // the combination function
  private def phi(start: Model, aModel: Model, bModel: Model): Model = {
    start.zip(aModel).zip(bModel).map { case ((s, a), b) =>
      val mutA = a ^ s
      val mutB = b ^ s
      val combMut = mutA | mutB
      s ^ combMut
    }
  }

  private def modelToAssignments(model: Model): List[(String, BigInt)] =
    model.zip(support).map{ case (value, sym) => sym.name -> value }

  private def getRandomAssignment: Model = support.map(sym => BigInt(sym.width, random))

  private def readModel(): Model = support.map { solver.getValue(_).get }

  // this models the SMTbit approach where every bit is individually constrained
  private def assignSoft(values: Model): Unit = {
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
  private def isValid(model: Model): Boolean = {
    val mapping = support.zip(model).map(t => t._1.name -> t._2).toMap
    val ctx = smt.LocalEvalCtx(mapping)
    val res = constraints.map(c => smt.SMTExprEval.eval(c)(ctx))
    res.forall(_ == 1)
  }

  private val shifts = support.map(_.width).dropRight(1)
  private val singleVar = support.size == 1
  private def modelToBigInt(model: Model): BigInt = {
    if(singleVar) { model.head } else {
      model.tail.zip(shifts).foldLeft(model.head) { case (prev, (value, shift)) =>
        (prev << shift) | value
      }
    }
  }
}

object SMTSampler {
  val Default = SMTSamplerOptions(alphaMin = 0.5, maxCombine = 4, maxSamples = 100)
  def apply(support: List[smt.BVSymbol], constraints: List[smt.BVExpr], opt: SMTSamplerOptions = Default, seed: Long = 0): Option[SMTSampler] = {
    val solver = new smt.solvers.Z3SMTLib()
    solver.setLogic(smt.solvers.QF_BV)
    // declare all variables in the support
    support.foreach(s => solver.runCommand(smt.solvers.DeclareFunction(s, Seq())))
    // add all hard constraints
    constraints.foreach(c => solver.assert(c))
    // sanity check to see if the formula is actually satisfiable (if not, there is no use in trying to sample it)
    solver.check() match {
      case IsSat =>
        val random = new scala.util.Random(seed)
        Some(new SMTSampler(solver, opt, support, constraints, random))
      case _ => None
    }
  }
}
