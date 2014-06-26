package propositional.schema

import parsing._
import propositional._
import propositional.schema._

class FormulaSchemaTestSuite extends ParsableTestSuite[FormulaSchema] {
  val parsable = FormulaSchema
  val parameters = FormulaSchemaTestParameters
}

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
  val tokens =
    Set("(", ")", "¬") ++
      ConnectiveSchemaTestParameters.tokens
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
  val testParses = List(
    TestParse[FormulaSchema](Some("((F ∨ G) ∨ F)"), None, None, 
    Some(CompoundSchema(
      SpecifiedConnectiveSchema(Or),
      CompoundSchema(
        SpecifiedConnectiveSchema(Or),
        ArbitraryFormulaSchema("F"),
        ArbitraryFormulaSchema("G")),
      ArbitraryFormulaSchema("F")))))
}