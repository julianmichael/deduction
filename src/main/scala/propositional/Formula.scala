package propositional

import parsing._

// recursively defined propositional formulas
sealed abstract class Formula {
  // in case we want to extract the smallest necessary propositional signature
  lazy val signature: Set[String] = this match {
    case Atom(a)           => Set(a)
    case Negation(f)       => f.signature
    case Compound(_, f, g) => f.signature ++ g.signature
  }
  override val toString = this match {
    case Atom(a)            => a
    case Negation(f)        => s"¬$f"
    case Compound(op, f, g) => s"($f $op $g)"
  }
}
case object Formula extends ComplexParsable[Formula] {
  val synchronousProductions: Map[List[Parsable[_]], (List[AST[Parsable[_]]] => Option[Formula])] = Map(
    List(AtomicFormula) ->
      (c => for {
        w <- AtomicFormula.fromAST(c(0))
      } yield Atom(w)),
    List(Terminal("¬"), Formula) ->
      (c => for {
        f <- fromAST(c(1))
      } yield Negation(f)),
    List(Terminal("("), Formula, Connective, Formula, Terminal(")")) ->
      (c => for {
        f <- fromAST(c(1))
        op <- Connective.fromAST(c(2))
        g <- fromAST(c(3))
      } yield Compound(op, f, g)))
}

// the types of formula
case class Atom(a: String) extends Formula
case class Negation(f: Formula) extends Formula
case class Compound(c: Connective, f: Formula, g: Formula) extends Formula
