package deduction

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
  val startSymbol = "S"
  val synchronousProductions: Map[List[Parsable[_]], (List[AST] => Option[Sequent])] = Map(
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

case class Assumptions(set: Set[Formula]) {
  override def toString = set.mkString(", ")
}
case object Assumptions extends ComplexParsable[Assumptions] {
  override val startSymbol = "A"
  val synchronousProductions: Map[List[Parsable[_]], (List[AST] => Option[Assumptions])] = Map(
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
