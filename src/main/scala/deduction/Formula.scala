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
    case Atom(a)            => a
    case Negation(f)        => s"¬$f"
    case Compound(op, f, g) => s"($f $op $g)"
  }
}
object Formula extends Parsable[Formula] {
  override val startSymbol = "F"
  override val openSymbols = Set("a")
  override val children = Set[Parsable[_]](Connective)
  val synchronousProductions = Map[String, (List[AST] => Option[Formula])](
    "F -> a" ->
      (c => Some(Atom((c(0).children(0).label)))),
    "F -> ¬ F" ->
      (c => for {
        f <- fromAST(c(1))
      } yield Negation(f)),
    "F -> ( F C F )" ->
      (c => for {
        f <- fromAST(c(1))
        op <- Connective.fromAST(c(2))
        g <- fromAST(c(3))
      } yield Compound(op, f, g))).map { case (k, v) => (Production.fromString(k).get, v) }
}

// the types of formula
case class Atom(a: String) extends Formula
case class Negation(f: Formula) extends Formula
case class Compound(c: Connective, f: Formula, g: Formula) extends Formula

// the propositional connectives
sealed abstract class Connective
object Connective extends Parsable[Connective] {
  override val startSymbol = "C"
  val synchronousProductions = Map[String, (List[AST] => Option[Connective])](
    "C -> ∧" -> (c => if (c(0).label == "∧") Some(And) else None),
    "C -> ∨" -> (c => if (c(0).label == "∨") Some(Or) else None),
    "C -> →" -> (c => if (c(0).label == "→") Some(Implies) else None)).map { case (k, v) => (Production.fromString(k).get, v) }
  override val children = Set[Parsable[_]]()

  private val stringMap: Map[String, Connective] = Map(
    "∧" -> And,
    "∨" -> Or,
    "→" -> Implies)
}
case object And extends Connective {
  override val toString = "∧"
}
case object Or extends Connective {
  override val toString = "∨"
}
case object Implies extends Connective {
  override val toString = "→"
}
