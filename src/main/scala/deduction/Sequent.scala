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
object Sequent extends Parsable[Sequent] {
  override val startSymbol = "S"
  val synchronousProductions = Map[String, (List[AST] => Option[Sequent])](
    "S -> A ⇒ F" ->
      (c => for {
        a <- Assumptions.fromAST(c(0))
        f <- Formula.fromAST(c(2))
      } yield Sequent(a, Some(f))),
    "S -> A ⇒" ->
      (c => for {
        a <- Assumptions.fromAST(c(0))
      } yield Sequent(a, None)),
    "S -> ⇒ F" ->
      (c => for {
        f <- Formula.fromAST(c(1))
      } yield Sequent(Assumptions(Set()), Some(f)))).map { case (k, v) => (Production.fromString(k).get, v) }
  override val children = Set[Parsable[_]](Assumptions, Formula)
}

case class Assumptions(set: Set[Formula]) {
  override def toString = set.mkString(", ")
}
object Assumptions extends Parsable[Assumptions] {
  override val startSymbol = "A"
  val synchronousProductions = Map[String, (List[AST] => Option[Assumptions])](
    "A -> F" ->
      (c => for {
        f <- Formula.fromAST(c(0))
      } yield Assumptions(Set(f))),
    "A -> A , F" ->
      (c => for {
        a <- fromAST(c(0))
        f <- Formula.fromAST(c(2))
      } yield (Assumptions(a.set + f)))).map { case (k, v) => (Production.fromString(k).get, v) }
  override val children = Set[Parsable[_]](Formula)

}
