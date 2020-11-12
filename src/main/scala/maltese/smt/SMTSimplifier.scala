// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.smt

object SMTSimplifier {

  /** Recursively simplifies expressions from bottom to top. */
  def simplify(expr: SMTExpr): SMTExpr = expr.mapExpr(simplify) match {
    // constant folding
    case u : BVUnaryExpr if isLit(u.e) => constantFold(u)
    case b : BVBinaryExpr if isLit(b.a) && isLit(b.b)  => constantFold(b)
    case op: BVOp => simplifyOp(op)
    case eq: BVEqual => simplifyBVEqual(eq)
    case BVExtend(e, 0, _) => e
    case slice: BVSlice => simplifySlice(slice)
    case BVNot(BVNot(e)) => e
    case ite: BVIte => simplifyBVIte(ite)
    case other => other
  }

  private def isLit(e: SMTExpr): Boolean = e match {
    case BVLiteral(_, _) => true
    case _ => false
  }

  private def simplifyBVEqual(expr: BVEqual): BVExpr = (expr.a, expr.b) match {
    case (a, b) if a == b => True()
    case (a, True()) => a
    case (True(), a) => a
    case (a, False()) => not(a)
    case (False(), a) => not(a)
    case (BVConcat(msb, lsb), other) => splitBVEqual(msb, lsb, other)
    case (other, BVConcat(msb, lsb)) => splitBVEqual(msb, lsb, other)
    case (_, _) => expr
  }

  private def splitBVEqual(msb: BVExpr, lsb: BVExpr, other: BVExpr): BVExpr = {
    // adding a slice to the other value can enable simplifications
    val otherLsb = BVSlice(other, lsb.width - 1, 0)
    val otherMsb = BVSlice(other, other.width - 1, lsb.width)
    // the new sub equalities could also have simplification opportunities, e.g., because of nested Concat
    simplify(and(BVEqual(msb, otherMsb), BVEqual(lsb, otherLsb))).asInstanceOf[BVExpr]
  }

  private def simplifyBVIte(i: BVIte): BVExpr = (i.cond, i.tru, i.fals) match {
    // constant condition
    case (True(), tru, _) => tru
    case (False(), _, fals) => fals

    // same result
    case (_, tru, fals) if tru == fals => tru

    // boolean result (all verified with sympy)
    // simplify_logic(ITE(c, 1, 0)) = c
    case (cond, True(), False()) => cond
    // simplify_logic(ITE(c, 0, 1)) = ~c
    case (cond, False(), True()) => BVNot(cond)
    // simplify_logic(ITE(c, 1, b)) = b | c
    case (cond, True(), b) => or(cond, b)
    // simplify_logic(ITE(c, 0, b)) = b & ~c
    case (cond, False(), b) => and(not(cond), b)
    // simplify_logic(ITE(c, b, 1)) = b | ~c
    case (cond, b, True()) => or(not(cond), b)
    // simplify_logic(ITE(c, b, 0)) = b & c
    case (cond, b, False()) => and(cond, b)

    // nested ite
    case (condA, BVIte(condB, truB, falsB), falsA) if falsA == falsB => BVIte(and(condA, condB), truB, falsA)
    case _ => i
  }

  private def simplifyOp(expr: BVOp): BVExpr = {
    if(expr.width == 1) { simplifyBoolOp(expr) } else { expr match {
      case BVOp(Op.And, a, mask: BVLiteral) => simplifyBitMask(expr, a, mask.value)
      case BVOp(Op.And, mask: BVLiteral, a) => simplifyBitMask(expr, a, mask.value)
      case other => other
    }}
  }

  private def simplifyBoolOp(expr: BVOp): BVExpr = expr match {
    case BVOp(Op.And, a, True()) => a
    case BVOp(Op.And, True(), b) => b
    case BVOp(Op.And, _, False()) => BVLiteral(0, 1)
    case BVOp(Op.And, False(), _) => BVLiteral(0, 1)
    case BVOp(Op.Or, _, True()) => BVLiteral(1, 1)
    case BVOp(Op.Or, True(), _) => BVLiteral(1, 1)
    case BVOp(Op.Or, a, False()) => a
    case BVOp(Op.Or, False(), b) => b
    case other => other
  }

  private val MaxRanges = 16
  private def simplifyBitMask(old: BVExpr, expr: BVExpr, mask: BigInt): BVExpr = {
    val ranges = maskToRanges(mask, old.width)
    if(ranges.size > MaxRanges) { old } else {
      ranges.reverseIterator.map {
        case (msb, lsb, true) => simplify(BVSlice(expr, hi=msb, lo=lsb)).asInstanceOf[BVExpr]
        case (msb, lsb, false) => BVLiteral(0, msb - lsb + 1)
      }.reduce((a, b) => BVConcat(a, b))
    }
  }

  private def maskToRanges(mask: BigInt, width: Int): Seq[(Int, Int, Boolean)] =
  if(width == 0) { List() } else if(width == 1) { List((0, 0, mask == 1)) } else {
    var lsb: Int = 0
    var lastBit: Boolean = (mask & 1) == 1
    (1 until width).flatMap { ii =>
      val bit = ((BigInt(1) << ii) & mask) != 0
      if(lastBit == bit) { None } else {
        val r = (ii-1, lsb, lastBit)
        lastBit = bit
        lsb = ii
        Some(r)
      }
    } :+ (width - 1, lsb, lastBit)
  }

  private def simplifySlice(expr: BVSlice): BVExpr = expr match {
    // no-op
    case BVSlice(e, hi, 0) if hi == e.width - 1 => e
    // slice of slice
    case BVSlice(BVSlice(e, _, innerLo), hi, lo) => combineSlice(e, innerLo, hi=hi, lo=lo)
    // slice of concat (this can enable new simplifications)
    // TODO: we can probably make this a bit more performant by only performing top-down instead of bottom up
    //       simplifications since the leaves are already simplified.
    case BVSlice(BVConcat(msb, lsb), hi, lo) =>
      simplify(pushDownSlice(msb, lsb, hi, lo)).asInstanceOf[BVExpr]
    // push slice into ite (this can enable new simplifications)
    case BVSlice(BVIte(cond, tru, fals), hi, lo) =>
      simplify(BVIte(cond, BVSlice(tru, hi, lo), BVSlice(fals, hi, lo))).asInstanceOf[BVExpr]
    case other => other
  }

  private def combineSlice(expr: BVExpr, innerLo: Int, hi: Int, lo: Int): BVSlice = {
    val combinedLo = lo + innerLo
    val combinedHi = hi + innerLo
    BVSlice(expr, hi=combinedHi, lo=combinedLo)
  }

  // we try to "push" slice expressions as far down as possible
  // e.g. concat(1'b1, 1'b0)[0] => 1'b0
  private def pushDownSlice(msb: BVExpr, lsb: BVExpr, hi: Int, lo: Int): BVExpr = {
    if(lsb.width > hi) { BVSlice(lsb, hi, lo)
    } else if(lo >= lsb.width) { BVSlice(msb, hi - lsb.width, lo - lsb.width)
    } else {
      BVConcat(
        BVSlice(msb, hi - lsb.width, 0),
        BVSlice(lsb, lsb.width - 1, lo)
      )
    }
  }

  private def constantFold(expr: BVUnaryExpr): BVLiteral = {
    // we cannot use pattern matching here since it does not support BigInts
    val value = expr.e.asInstanceOf[BVLiteral].value
    val r: BigInt = expr match {
      case BVExtend(BVLiteral(_, width), by, signed) => SMTExprEval.doBVExtend(value, width, by, signed)
      case BVSlice(_, hi, lo) => SMTExprEval.doBVSlice(value, hi=hi, lo=lo)
      case BVNot(_) => SMTExprEval.doBVNot(value, expr.width)
      case BVNegate(_) => SMTExprEval.doBVNegate(value, expr.width)
      case other => throw new NotImplementedError(s"Unexpected expression: $other")
    }
    BVLiteral(r, expr.width)
  }

  private def constantFold(expr: BVBinaryExpr): BVLiteral = {
    // we cannot use pattern matching here since it does not support BigInts
    val a = expr.a.asInstanceOf[BVLiteral].value
    val b = expr.b.asInstanceOf[BVLiteral].value

    val r: BigInt = expr match {
      case _: BVEqual => SMTExprEval.doBVEqual(a, b)
      case BVOp(op, _, _) => SMTExprEval.doBVOp(op, a, b, expr.width)
      case BVComparison(op, _, _, signed) => SMTExprEval.doBVCompare(op, a, b, expr.a.width, signed)
      case _: BVConcat => SMTExprEval.doBVConcat(a, b, bWidth = expr.b.width)
      case other => throw new NotImplementedError(s"Unexpected expression: $other")
    }
    BVLiteral(r, expr.width)
  }

  private def and(a: BVExpr, b: BVExpr): BVOp = BVOp(Op.And, a, b)
  private def or(a: BVExpr, b: BVExpr): BVOp = BVOp(Op.Or, a, b)
  def not(e: BVExpr): BVExpr = e match { case BVNot(inner) => inner case o => BVNot(o) }
}
