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
case object Formula extends ComplexParsable[Formula] {
  val startSymbol = "F"
  val synchronousProductions: Map[List[Parsable[_]], (List[AST] => Option[Formula])] = Map(
    List(Word) ->
      (c => for {
        w <- Word.fromAST(c(0))
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

// the propositional connectives
sealed abstract class Connective
case object Connective extends ComplexParsable[Connective] {
  override val startSymbol = "C"
  val synchronousProductions: Map[List[Parsable[_]], (List[AST] => Option[Connective])] = Map(
    List(Terminal(And.toString)) ->
      (c => for {
        op <- Terminal(And.toString).fromAST(c(0))
      } yield And),
    List(Terminal(Or.toString)) ->
      (c => for {
        op <- Terminal(Or.toString).fromAST(c(0))
      } yield Or),
    List(Terminal(Implies.toString)) ->
      (c => for {
        op <- Terminal(Implies.toString).fromAST(c(0))
      } yield Implies))

  // as of yet unused
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
