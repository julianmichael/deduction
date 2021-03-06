package propositional.schema 

import parsing._
import propositional._
import deduction.Schema

sealed abstract class FormulaSchema extends Schema[Formula]
case object FormulaSchema extends ComplexParsable[FormulaSchema] {
  val synchronousProductions: Map[List[Parsable[_]], (List[AST[Parsable[_]]] => Option[FormulaSchema])] = Map(
    List(Name) -> 
      (c => for {
        w <- Name.fromAST(c(0))
      } yield ArbitraryFormulaSchema(w)),
    List(Terminal("¬"), FormulaSchema) -> 
      (c => for {
        f <- FormulaSchema.fromAST(c(1))
      } yield NegationSchema(f)),
    List(Terminal("("), FormulaSchema, ConnectiveSchema, FormulaSchema, Terminal(")")) -> 
      (c => for {
        f <- FormulaSchema.fromAST(c(1))
        op <- ConnectiveSchema.fromAST(c(2))
        g <- FormulaSchema.fromAST(c(3))
      } yield CompoundSchema(op, f, g))
  )
}
case class ArbitraryFormulaSchema(name: String) extends FormulaSchema {
  override def hasMatch(targ: Formula) = true
  override def matches(targ: Formula) = Map(name -> targ) :: Nil
  override val toString = name
}

// as of yet unused
case class AtomSchema(name: String) extends FormulaSchema {
  override def hasMatch(targ: Formula) = targ match {
    case Atom(_) => true
    case _ => false
  }
  override def matches(targ: Formula) = targ match {
    case Atom(a) => Map(name -> a) :: Nil
    case _       => Nil
  }
  override val toString = name
}
case class NegationSchema(f: FormulaSchema) extends FormulaSchema {
  override def hasMatch(targ: Formula) = targ match {
    case Negation(negated) => f.hasMatch(negated)
    case _ => false
  }
  override def matches(targ: Formula) = targ match {
    case Negation(negated) => f.matches(negated)
    case _                 => Nil
  }
  override val toString = s"¬$f"
}
case class CompoundSchema(c: ConnectiveSchema, f: FormulaSchema, g: FormulaSchema) extends FormulaSchema {
  override def hasMatch(targ: Formula) = targ match {
    case Compound(op, first, second) =>
      c.hasMatch(op) && f.hasMatch(first) && g.hasMatch(second) && !matches(targ).isEmpty
    case _ => false
  }
  override def matches(targ: Formula) = targ match {
    case Compound(op, first, second) => {
      val connectiveNamings = c.matches(op)
      val firstNamings = f.matches(first)
      val secondNamings = g.matches(second)
      consistentNamings(connectiveNamings :: firstNamings :: secondNamings :: Nil)
    }
    case _ => Nil
  }
  override val toString = s"($f $c $g)"
}