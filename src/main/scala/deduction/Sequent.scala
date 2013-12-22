package deduction

case class Sequent(assumptions: Set[Formula], conclusion: Option[Formula]) {
  override def toString = {
    val assumptionsString = assumptions.mkString(", ")
    val conclusionString = conclusion match {
      case None => ""
      case Some(f) => f.toString
    }
    s"$assumptionsString ⇒ $conclusionString"
  }
}
object Sequent {
  val func = ((c: List[AST]) => assumptionsFromAST(c(0)).flatMap { a => Some(new Sequent(a, None)) })
  private val synchronousSequentProductions = Map[String, (List[AST] => Option[Sequent])](
    "S -> A ⇒ F" -> (c => (assumptionsFromAST(c(0)), Formula.fromAST(c(2))) match {
      case (Some(a), Some(f)) => Some(Sequent(a, Some(f)))
      case _                  => None
    }),
    "S -> A ⇒" -> (c => assumptionsFromAST(c(0)) flatMap {
      a => Some(Sequent(a, None))
    }),
    "S -> ⇒ F" -> (c => Formula.fromAST(c(1)) flatMap {
      f => Some(Sequent(Set(), Some(f)))
    })).map { case (k, v) => (Production.fromString(k).get, v) }
  private val synchronousAssumptionProductions = Map[String, (List[AST] => Option[Set[Formula]])](
    "A -> F" -> (c => Formula.fromAST(c(0)) flatMap {
      f => Some(Set(f))
    }),
    "A -> A , F" -> (c => (assumptionsFromAST(c(0)), Formula.fromAST(c(2))) match {
      case (Some(a), Some(f)) => Some(a + f)
      case _                  => None
    })).map { case (k, v) => (Production.fromString(k).get, v) }

  val productions = synchronousSequentProductions.keySet ++ synchronousAssumptionProductions.keySet
  val grammar = new Grammar(Formula.productions ++ productions, Some("S"), Some(Formula.atomSymbol))
  
  def fromString(s: String) = grammar.parse(s) flatMap fromAST
  def fromAST(ast: AST): Option[Sequent] = sequentFromAST(ast)
  private def assumptionsFromAST(ast: AST): Option[Set[Formula]] = {
    ast.production flatMap {
      p => synchronousAssumptionProductions(p)(ast.children)
    }
  }
  private def sequentFromAST(ast: AST): Option[Sequent] = {
    ast.production flatMap {
      p => synchronousSequentProductions(p)(ast.children)
    }
  }
}
