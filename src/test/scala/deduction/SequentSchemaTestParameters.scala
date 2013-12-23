package deduction

object SequentSchemaTestParameters extends ParsableTestParameters[SequentSchema] {
  val children = Set[Parsable[_]](
    AssumptionSchema,
    FormulaSchema,
    Terminal("⇒")) ++
    AssumptionSchemaTestParameters.children ++
    FormulaSchemaTestParameters.children
  val nonterminals =
    Set("SS") ++
      AssumptionSchemaTestParameters.nonterminals ++
      FormulaSchemaTestParameters.nonterminals
  val terminals =
    Set("⇒") ++
      AssumptionSchemaTestParameters.terminals ++
      FormulaSchemaTestParameters.terminals
  val productions = Set[Production](
    RawProduction("SS", List("AS", "⇒", "FS")),
    RawProduction("SS", List("AS", "⇒")),
    RawProduction("SS", List("⇒", "FS"))) ++
    AssumptionSchemaTestParameters.productions ++
    FormulaSchemaTestParameters.productions
  val cnfProductions = Set[CNFProduction](
    Binary("SS", "AS", "{⇒+FS}"),
    ChunkedBinary("{⇒+FS}", "⇒", "FS"),
    Binary("SS", "AS", "⇒"),
    Binary("SS", "⇒", "FS")) ++
    AssumptionSchemaTestParameters.cnfProductions ++
    FormulaSchemaTestParameters.cnfProductions
  val goodStrings = List(
    "F ⇒ F")
  val goodTokenizations = Nil
  val goodASTs = Nil
  val goodSymbolics = List(
    SequentSchema(
      SingleFormulaSchema(
        ArbitraryFormulaSchema("F")),
      Some(ArbitraryFormulaSchema("F"))))
}