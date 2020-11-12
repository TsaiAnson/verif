// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>


package firrtl.backends.experimental.smt

import firrtl.AnnotationSeq
import firrtl.ir
import maltese.mc
import maltese.{smt => m}

/** converts between firrtl's internal SMT expr library and the maltese expression library */
object ExpressionConverter {
  private case class Context() extends TranslationContext {
    override def getRandom(width: Int): BVExpr = BVSymbol("Random", width)
  }
  def toMaltese(e: ir.Expression, width: Int, allowNarrow: Boolean): m.BVExpr = {
    implicit val ctx: TranslationContext = Context()
    val firSmt = FirrtlExpressionSemantics.toSMT(e, width, allowNarrow)
    toMaltese(firSmt)
  }

  def toMaltese(annos: AnnotationSeq): Option[mc.TransitionSystem] =
    annos.collectFirst { case TransitionSystemAnnotation(sys) => sys }.map(toMaltese)

  def toMaltese(sys: TransitionSystem): mc.TransitionSystem = {
    val inputs = sys.inputs.map(toMaltese)
    val states = sys.states.map(toMaltese)
    val signals = sys.signals.map { s =>
      val lbl = if(sys.outputs.contains(s.name)) { mc.IsOutput
      } else if(sys.assumes.contains(s.name)) {    mc.IsConstraint
      } else if(sys.asserts.contains(s.name)) {    mc.IsBad
      } else if(sys.fair.contains(s.name)) {       mc.IsFair
      } else { mc.IsNode }
      val eMaltese = toMaltese(s.e)
      val expr = if(lbl == mc.IsBad) m.BVNot(eMaltese) else eMaltese
      mc.Signal(s.name, expr, lbl)
    }
    mc.TransitionSystem(sys.name, inputs.toList, states.toList, signals.toList)
  }

  private def toMaltese(state: State): mc.State = {
    mc.State(
      sym = toMaltese(state.sym),
      init = state.init.map(toMaltese),
      next = state.next.map(toMaltese)
    )
  }

  def toMaltese(expr: SMTExpr): m.SMTExpr = expr match {
    case b: BVExpr => toMaltese(b)
    case a: ArrayExpr => toMaltese(a)
  }

  def toMaltese(expr: BVExpr): m.BVExpr = expr match {
    case BVLiteral(value, width) => m.BVLiteral(value, width)
    case BVSymbol(name, width) => m.BVSymbol(name, width)
    case BVExtend(e, by, signed) => m.BVExtend(toMaltese(e), by, signed)
    case BVSlice(e, hi, lo) => m.BVSlice(toMaltese(e), hi, lo)
    case BVNot(e) => m.BVNot(toMaltese(e))
    case BVNegate(e) => m.BVNegate(toMaltese(e))
    case r: BVReduceOr => toMaltese(Expander.expand(r))
    case r: BVReduceAnd => toMaltese(Expander.expand(r))
    case r: BVReduceXor => toMaltese(Expander.expand(r))
    case BVImplies(a, b) => toMaltese(implies(a, b))
    case BVEqual(a, b) => m.BVEqual(toMaltese(a), toMaltese(b))
    case BVComparison(op, a, b, signed) => m.BVComparison(toMalteseCmp(op), toMaltese(a), toMaltese(b), signed)
    case BVOp(op, a, b) => m.BVOp(toMalteseOp(op), toMaltese(a), toMaltese(b))
    case BVConcat(a, b) => m.BVConcat(toMaltese(a), toMaltese(b))
    case ArrayRead(array, index) => m.ArrayRead(toMaltese(array), toMaltese(index))
    case BVIte(cond, tru, fals) => m.BVIte(toMaltese(cond), toMaltese(tru), toMaltese(fals))
    case ArrayEqual(a, b) => m.ArrayEqual(toMaltese(a), toMaltese(b))
    case BVRawExpr(serialized, _) => throw new NotImplementedError(s"Unsupported RawExpr: $serialized")
  }

  private def implies(a: BVExpr, b: BVExpr): BVExpr = BVOp(Op.Or, BVNot(a), b)

  def toMaltese(sym: BVSymbol): m.BVSymbol = m.BVSymbol(sym.name, sym.width)

  def toMaltese(sym: SMTSymbol): m.SMTSymbol = sym match {
    case BVSymbol(name, width) => m.BVSymbol(name, width)
    case ArraySymbol(name, indexWidth, dataWidth) => m.ArraySymbol(name, indexWidth, dataWidth)
  }
  
  private def toMalteseCmp(op: Compare.Value): m.Compare.Value = op match {
    case Compare.Greater => m.Compare.Greater
    case Compare.GreaterEqual => m.Compare.GreaterEqual
  }
  
  private def toMalteseOp(op: Op.Value): m.Op.Value = op match {
    case Op.And =>  m.Op.And
    case Op.Or => m.Op.Or
    case Op.Xor => m.Op.Xor
    case Op.ShiftLeft => m.Op.ShiftLeft
    case Op.ArithmeticShiftRight => m.Op.ArithmeticShiftRight
    case Op.ShiftRight => m.Op.ShiftRight
    case Op.Add => m.Op.Add
    case Op.Mul => m.Op.Mul
    case Op.SignedDiv => m.Op.SignedDiv
    case Op.UnsignedDiv => m.Op.UnsignedDiv
    case Op.SignedMod => m.Op.SignedMod
    case Op.SignedRem => m.Op.SignedRem
    case Op.UnsignedRem => m.Op.UnsignedRem
    case Op.Sub => m.Op.Sub
  }

  def toMaltese(expr: ArrayExpr): m.ArrayExpr = expr match {
    case ArraySymbol(name, indexWidth, dataWidth) => m.ArraySymbol(name, indexWidth, dataWidth)
    case ArrayStore(array, index, data) => m.ArrayStore(toMaltese(array), toMaltese(index), toMaltese(data))
    case ArrayIte(cond, tru, fals) => m.ArrayIte(toMaltese(cond), toMaltese(tru), toMaltese(fals))
    case ArrayConstant(e, indexWidth) => m.ArrayConstant(toMaltese(e), indexWidth)
    case ArrayRawExpr(serialized, _, _) => throw new NotImplementedError(s"Unsupported RawExpr: $serialized")
  }

}
