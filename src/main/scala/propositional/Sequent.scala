package propositional

import parsing._

case class Sequent(assumptions: Assumptions, conclusion: Option[Formula]) {
  override def toString = {
    val conclusionString = conclusion match {
      case None    => ""
      case Some(f) => f.toString
    }
    s"$assumptions ⇒ $conclusionString"
  }
}
case object Sequent extends ComplexParsable[Sequent] {
  val synchronousProductions: Map[List[Parsable[_]], (List[AST[Parsable[_]]] => Option[Sequent])] = Map(
    List(Assumptions, Terminal("⇒"), Formula) ->
      (c => for {
        a <- Assumptions.fromAST(c(0))
        f <- Formula.fromAST(c(2))
      } yield Sequent(a, Some(f))),
    List(Assumptions, Terminal("⇒")) ->
      (c => for {
        a <- Assumptions.fromAST(c(0))
      } yield Sequent(a, None)),
    List(Terminal("⇒"), Formula) ->
      (c => for {
        f <- Formula.fromAST(c(1))
      } yield Sequent(Assumptions(Set()), Some(f))))
}
