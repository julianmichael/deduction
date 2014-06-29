package propositional

import parsing._

case class Assumptions(set: Set[Formula]) {
  override def toString = set.mkString(", ")
}
case object Assumptions extends ComplexParsable[Assumptions] {
  val synchronousProductions: Map[List[Parsable[_]], (List[AST[Parsable[_]]] => Option[Assumptions])] = Map(
    List(Formula) ->
      (c => for {
        f <- Formula.fromAST(c(0))
      } yield Assumptions(Set(f))),
    List(Assumptions, Terminal(","), Formula) ->
      (c => for {
        a <- fromAST(c(0))
        f <- Formula.fromAST(c(2))
      } yield (Assumptions(a.set + f))))
}