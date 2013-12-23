package deduction

object AssumptionsTestParameters extends ParsableTestParameters[Assumptions] {
  val nonterminals = Set("A") ++ FormulaTestParameters.nonterminals
  val terminals = Set(",") ++ FormulaTestParameters.terminals
  val productions = Set[Production](
    RawProduction("A", List("F")),
    RawProduction("A", List("A", ",", "F"))) ++
    FormulaTestParameters.productions
  val cnfProductions = Set[CNFProduction](
    Unary("A", "F"),
    Binary("A", "A", "{,+F}"),
    ChunkedBinary("{,+F}", ",", "F")) ++
    FormulaTestParameters.cnfProductions
  val goodStrings = Nil
  val goodTokenizations = Nil
  val goodASTs = Nil
  val goodSymbolics = Nil
}