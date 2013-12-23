package deduction

object FormulaSchemaTestParameters extends ParsableTestParameters[FormulaSchema] {
  val children = Set[Parsable[_]](
    ConnectiveSchema,
    Word,
    Terminal("("),
    Terminal(")"),
    Terminal("¬")) ++
    ConnectiveSchemaTestParameters.children
  val nonterminals =
    Set("FS") ++
      ConnectiveSchemaTestParameters.nonterminals
  val terminals =
    Set("(", ")", "¬") ++
      ConnectiveSchemaTestParameters.terminals
  val productions = Set[Production](
    RawProduction("FS", List("w")),
    RawProduction("FS", List("¬", "FS")),
    RawProduction("FS", List("(", "FS", "CS", "FS", ")"))) ++
    ConnectiveSchemaTestParameters.productions
  val cnfProductions = Set[CNFProduction](
    Unary("FS", "w"),
    Binary("FS", "¬", "FS"),
    Binary("FS", "(", "{FS+CS+FS+)}"),
    ChunkedBinary("{FS+CS+FS+)}", "FS", "{CS+FS+)}"),
    ChunkedBinary("{CS+FS+)}", "CS", "{FS+)}"),
    ChunkedBinary("{FS+)}", "FS", ")")) ++
    ConnectiveSchemaTestParameters.cnfProductions
  val goodStrings = List(
    "((F ∨ G) ∨ F)")
  val goodTokenizations = Nil
  val goodASTs = Nil
  val goodSymbolics = List(
    CompoundSchema(
      SpecifiedConnectiveSchema(Or),
      CompoundSchema(
        SpecifiedConnectiveSchema(Or),
        ArbitraryFormulaSchema("F"),
        ArbitraryFormulaSchema("G")),
      ArbitraryFormulaSchema("F")))
}