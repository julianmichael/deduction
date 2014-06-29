package propositional

import parsing._

sealed abstract class Connective
case object Connective extends ComplexParsable[Connective] {
  val synchronousProductions: Map[List[Parsable[_]], (List[AST[Parsable[_]]] => Option[Connective])] = Map(
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