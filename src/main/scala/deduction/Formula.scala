package deduction 

// recursively defined propositional formulas
sealed abstract class Formula {
  // in case we want to extract the smallest necessary propositional signature
  lazy val signature: Set[String] = this match {
    case Atom(a)           => Set(a)
    case Negation(f)       => f.signature
    case Compound(_, f, g) => f.signature ++ g.signature
  }
  override val toString = this match {
    case Atom(a) => a
    case Negation(f) => s"¬$f"
    case Compound(op, f, g) => s"($f $op $g)"
  }
}
object Formula {
  val atomSymbol = "a"

  private val synchronousFormulaProductions = Map[String, (List[AST] => Option[Formula])](
    s"F -> $atomSymbol" -> (c => Some(Atom((c(0).children(0).label)))),
    "F -> ¬ F" -> (c => formulaFromAST(c(1)) flatMap {
      f => Some(Negation(f))
    }),
    "F -> ( F C F )" -> (c => (formulaFromAST(c(1)), connectiveFromAST(c(2)), formulaFromAST(c(3))) match {
      case (Some(f), Some(c), Some(g)) => Some(Compound(c, f, g))
      case _                           => None
    })).map { case (k, v) => (Production.fromString(k).get, v) }

  private val synchronousConnectiveProductions = Map[String, (List[AST] => Option[Connective])](
    "C -> ∧" -> (c => if (c(0).label == "∧") Some(And) else None),
    "C -> ∨" -> (c => if (c(0).label == "∨") Some(Or) else None),
    "C -> →" -> (c => if (c(0).label == "→") Some(Implies) else None)).map { case (k, v) => (Production.fromString(k).get, v) }

  val productions = synchronousFormulaProductions.keySet ++ synchronousConnectiveProductions.keySet
  val grammar = new Grammar(productions, Some("F"), Some(atomSymbol))

  def fromString(s: String) = grammar.parse(s) flatMap fromAST
  def fromAST(ast: AST) = formulaFromAST(ast)
  private def formulaFromAST(ast: AST): Option[Formula] = {
    ast.production flatMap {
      p => synchronousFormulaProductions.get(p) flatMap (_(ast.children))
    }
  }
  private def connectiveFromAST(ast: AST): Option[Connective] = {
    ast.production flatMap {
      p => synchronousConnectiveProductions.get(p) flatMap (_(ast.children))
    }
  }
}

// the types of formula
case class Atom(a: String) extends Formula
case class Negation(f: Formula) extends Formula
case class Compound(c: Connective, f: Formula, g: Formula) extends Formula
// the propositional connectives
sealed abstract class Connective
case object And extends Connective {
  override val toString = "∧"
}
case object Or extends Connective {
  override val toString = "∨"
}
case object Implies extends Connective {
  override val toString = "→"
}
