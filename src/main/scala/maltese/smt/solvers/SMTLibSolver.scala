// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.smt.solvers

import maltese.smt
import maltese.smt.solvers.Solver.Logic
import maltese.smt.solvers.uclid.InteractiveProcess

class Yices2SMTLib extends SMTLibSolver(List("yices-smt2", "--incremental")) {
  override def name = "yices2-smtlib"
  override def supportsConstArrays = false
  override def supportsUninterpretedFunctions = true
  override def supportsQuantifiers = false
}

class CVC4SMTLib extends SMTLibSolver(List("cvc4", "--incremental", "--produce-models", "--lang", "smt2")) {
  override def name = "cvc4-smtlib"
  override def supportsConstArrays = true
  override def supportsUninterpretedFunctions = true
  override def supportsQuantifiers = true
}

class Z3SMTLib extends SMTLibSolver(List("z3", "-in")) {
  override def name = "z3-smtlib"
  override def supportsConstArrays = true
  override def supportsUninterpretedFunctions = true
  override def supportsQuantifiers = true

  // Z3 only supports array (as const ...) when the logic is set to ALL
  override protected def doSetLogic(logic: Logic): Unit = getLogic match {
    case None => writeCommand("(set-logic ALL)")
    case Some(_) => // ignore
  }
}


/** provides basic facilities to interact with any SMT solver that supports a SMTLib base textual interface */
abstract class SMTLibSolver(cmd: List[String]) extends Solver {
  protected val debug: Boolean = false

  override def push(): Unit = {
    writeCommand("(push 1)")
  }
  override def pop(): Unit = {
    writeCommand("(pop 1)")
  }
  override def assert(expr: smt.BVExpr): Unit = {
    // TODO: println("TODO: declare free variables automatically")
    writeCommand(s"(assert ${serialize(expr)})")
  }
  override def queryModel(e: smt.BVSymbol): Option[BigInt] = getValue(e)
  override def getValue(e: smt.BVExpr): Option[BigInt] = {
    val cmd = s"(get-value (${serialize(e)}))"
    writeCommand(cmd)
    readResponse() match {
      case Some(strModel) => Some(SMTLibResponseParser.parseValue(strModel.trim))
      case None => throw new RuntimeException(s"Solver ${name} did not reply to $cmd")
    }
  }
  override def getValue(e: smt.ArrayExpr): Seq[(Option[BigInt], BigInt)] = {
    val cmd = s"(get-value (${serialize(e)}))"
    writeCommand(cmd)
    readResponse() match {
      case Some(strModel) => SMTLibResponseParser.parseMemValue(strModel.trim)
      case None => throw new RuntimeException(s"Solver ${name} did not reply to $cmd")
    }
  }
  override def runCommand(cmd: SMTCommand): Unit = cmd match {
    case Comment(_) => // ignore comments
    case SetLogic(logic) => setLogic(logic)
    case c: DefineFunction => writeCommand(serialize(c))
    case c: DeclareFunction => writeCommand(serialize(c))
    case c: DeclareUninterpretedSort => writeCommand(serialize(c))
    case c: DeclareUninterpretedSymbol => writeCommand(serialize(c))
  }

  /** releases all native resources */
  override def close(): Unit = {
    proc.finishInput()
    Thread.sleep(5)
    proc.kill()
  }
  override protected def doSetLogic(logic: Logic): Unit = getLogic match {
    case None => writeCommand(serialize(SetLogic(logic)))
    case Some(old) => require(logic == old, s"Cannot change logic from $old to $logic")
  }
  override protected def doCheck(produceModel: Boolean): SolverResult = {
    writeCommand("(check-sat)")
    readResponse() match {
      case Some(res) =>
        res.stripLineEnd match {
          case "sat" => IsSat
          case "unsat" => IsUnSat
          case other => throw new RuntimeException(s"Unexpected result from SMT solver: $other")
        }
      case None =>
        throw new RuntimeException("Unexpected EOF result from SMT solver.")
    }
  }

  private def serialize(e: smt.SMTExpr): String = smt.SMTLibSerializer.serialize(e)
  private def serialize(c: SMTCommand): String = smt.SMTLibSerializer.serialize(c)

  private val proc = new InteractiveProcess(cmd, true)
  protected def writeCommand(str : String): Unit = {
    if(debug) println(s"-> $str")
    proc.writeInput(str + "\n")
  }
  protected def readResponse() : Option[String] = {
    val r = proc.readOutput()
    if(debug) println(s"<- $r")
    r
  }
}


