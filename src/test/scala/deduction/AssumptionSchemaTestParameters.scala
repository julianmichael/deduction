package deduction

object AssumptionSchemaTestParameters extends ParsableTestParameters[AssumptionSchema] {
  val children = Set[Parsable[_]](
    FormulaSchema,
    Terminal("∪"),
    Terminal(",")) ++
    FormulaSchemaTestParameters.children
  val nonterminals = 
    Set("AS") ++
    FormulaSchemaTestParameters.nonterminals
  val terminals =
    Set(",", "∪") ++
    FormulaSchemaTestParameters.terminals
  val productions = Set[Production](
    RawProduction("AS", List("w")),
    RawProduction("AS", List("FS")),
    RawProduction("AS", List("AS", ",", "FS")),
    RawProduction("AS", List("AS", "∪", "AS"))) ++
    FormulaSchemaTestParameters.productions
  val cnfProductions = Set[CNFProduction](
    Unary("AS", "w"),
    Unary("AS", "FS"),
    Binary("AS", "AS", "{,+FS}"),
    ChunkedBinary("{,+FS}", ",", "FS"),
    Binary("AS", "AS", "{∪+AS}"),
    ChunkedBinary("{∪+AS}", "∪", "AS")) ++
    FormulaSchemaTestParameters.cnfProductions
  val goodStrings = Nil
  val goodTokenizations = Nil
  val goodASTs = Nil
  val goodSymbolics = Nil
}