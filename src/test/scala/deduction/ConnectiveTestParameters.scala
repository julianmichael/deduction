package deduction

object ConnectiveTestParameters extends ParsableTestParameters[Connective] {
  val children = Set[Parsable[_]](
      Terminal("∧"),
      Terminal("∨"),
      Terminal("→")
  )
  val nonterminals = Set("C")
  val terminals = Set("∧", "∨", "→")
  val productions = Set[Production](
    RawProduction("C", List("∧")),
    RawProduction("C", List("∨")),
    RawProduction("C", List("→")))
  val cnfProductions = Set[CNFProduction](
    Unary("C", "∧"),
    Unary("C", "∨"),
    Unary("C", "→"))
  val goodStrings = List(
    "∧",
    "∨",
    "→")
  // skip tokenizing
  val goodTokenizations = Nil
  val goodASTs = List(
    BasicAST("C", List(BasicAST("∧", Nil))),
    BasicAST("C", List(BasicAST("∨", Nil))),
    BasicAST("C", List(BasicAST("→", Nil))))
  val goodSymbolics = List(
    And,
    Or,
    Implies)
}