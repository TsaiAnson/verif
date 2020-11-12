// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.smt

object SMTExprEval {

  def doBVExtend(e: BigInt, width: Int, by: Int, signed: Boolean): BigInt = {
    if(signed && isNegative(e, width)) {
      (mask(by) << width) | e
    } else { e }
  }
  def doBVSlice(e: BigInt, hi: Int, lo: Int): BigInt = (e >> lo) & mask(hi - lo + 1)
  def doBVNot(e: BigInt, width: Int): BigInt = flipBits(e, width)
  def doBVNegate(e: BigInt, width: Int): BigInt = sub(0, e, width)
  def doBVEqual(a: BigInt, b: BigInt): BigInt = bool(a == b)
  def doBVCompare(op: Compare.Value, a: BigInt, b: BigInt, width: Int, signed: Boolean): BigInt = {
    if(a == b && op == Compare.GreaterEqual) return bool(true)

    // only checking for Greater than
    if(signed) {
      (isPositive(a, width), isPositive(b, width)) match {
        case (true, true) => doBVCompare(op, a, b, width - 1 , false)
        case (true, false) =>
          // a < 0 && b >= 0 => a can never be greater or equal to b
          bool(false)
        case (false, true) =>
          // a >= 0 && b < 0 => a is greater than b
          bool(true)
        case (false, false) => doBVCompare(op, doBVNegate(b, width), doBVNegate(a, width), width - 1, false)
      }
    } else {
      bool(a > b)
    }

  }
  def doBVOp(op: Op.Value, a: BigInt, b: BigInt, width: Int): BigInt = op match {
    case Op.And =>  a &  b
    case Op.Or => a | b
    case Op.Xor => a ^ b
    case Op.ShiftLeft => (a << b.toInt) & mask(width)
    case Op.ArithmeticShiftRight =>
      val by = b.toInt
      if(isPositive(a, width)) { a >> by
      } else if(by >= width) { mask(width)
      } else {
        val msb = mask(by) << (width - by)
        (a >> by) | msb
      }
    case Op.ShiftRight => a >> b.toInt
    case Op.Add => (a + b) & mask(width)
    case Op.Mul => (a * b) & mask(width)
    case Op.Sub => sub(a, b, width)
    case other => throw new NotImplementedError(other.toString)
  }
  def doBVConcat(a: BigInt, b: BigInt, bWidth: Int): BigInt = (a << bWidth) | b

  // helper functions
  private def sub(a: BigInt, b: BigInt, width: Int): BigInt = (a + flipBits(b, width) + 1) & mask(width)
  private def mask(width: Int): BigInt = (BigInt(1) << width) - 1
  private def isPositive(value: BigInt, w: Int) = (value & mask(w-1)) == value
  private def isNegative(value: BigInt, w: Int) = !isPositive(value, w)
  private def flipBits(value: BigInt, w: Int) = ~value & mask(w)
  private def bool(b: Boolean): BigInt = if(b) BigInt(1) else BigInt(0)
}
