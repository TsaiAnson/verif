// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.smt.solvers


object SMTLibResponseParser {
  def parseValue(v: String): BigInt = {
    val tree = SExprParser.parse(v)
    tree match {
      case SExprNode(List(SExprNode(List(_, SExprLeaf(valueStr))))) => parseBVLiteral(valueStr)
      case _ => throw new NotImplementedError(s"Unexpected response: $v")
    }
  }

  def parseMemValue(v: String): Seq[(Option[BigInt], BigInt)] = {
    val tree = SExprParser.parse(v)
    tree match {
      case SExprNode(List(SExprNode(List(_, value)))) => parseMem(value)
      case _ => throw new NotImplementedError(s"Unexpected response: $v")
    }
  }

  private def parseMem(value: SExpr): Seq[(Option[BigInt], BigInt)] = value match {
    case SExprNode(List(SExprNode(List(SExprLeaf("as"), SExprLeaf("const"), tpe)), SExprLeaf(valueStr))) =>
      // initialize complete memory to value
      List((None, parseBVLiteral(valueStr)))
    case other => throw new NotImplementedError(s"TODO: $value")
  }

  private def parseBVLiteral(valueStr: String): BigInt = {
    if(valueStr == "true") { BigInt(1) }
    else if(valueStr == "false") { BigInt(0) }
    else if(valueStr.startsWith("#b")) { BigInt(valueStr.drop(2), 2) }
    else if(valueStr.startsWith("#x")) { BigInt(valueStr.drop(2), 16) }
    else {
      throw new NotImplementedError(s"Unsupported number format: $valueStr")
    }
  }
}


sealed trait SExpr
case class SExprNode(children: List[SExpr]) extends SExpr {
  override def toString = children.mkString("(", " ", ")")
}
case class SExprLeaf(value: String) extends SExpr {
  override def toString = value
}

/** simple S-Expression parser to make sense of SMTLib solver output */
object SExprParser {
  def parse(line: String): SExpr = {
    val tokens = line
      .replace("(", " ( ")
      .replace(")", " ) ")
      .split("\\s+")
      .filterNot(_.isEmpty)
      .toList

    assert(tokens.nonEmpty)
    if(tokens.head == "(") {
      parseSExpr(tokens.tail)._1
    } else {
      assert(tokens.tail.isEmpty)
      SExprLeaf(tokens.head)
    }
  }

  private def parseSExpr(tokens: List[String]): (SExpr, List[String]) = {
    var t = tokens
    var elements = List[SExpr]()
    while(t.nonEmpty) {
      t.head match {
        case "(" =>
          val (child, nt) = parseSExpr(t.tail)
          t = nt
          elements = elements :+ child
        case ")" =>
          return (SExprNode(elements), t.tail)
        case other =>
          elements = elements :+ SExprLeaf(other)
          t = t.tail
      }
    }
    (SExprNode(elements), List())
  }
}
