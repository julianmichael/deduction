package propositional.schema

import parsing._
import propositional._
import propositional.schema._

class SequentSchemaTestSuite extends ParsableTestSuite[SequentSchema] {
  val parsable = SequentSchema
  val parameters = SequentSchemaTestParameters 
}

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
  val tokens =
    Set("⇒") ++
      AssumptionSchemaTestParameters.tokens ++
      FormulaSchemaTestParameters.tokens
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
  val testParses = List(
    TestParse[SequentSchema](
      Some("|F ⇒ F"),
      Some(List("|", "F", "⇒", "F")),
      Some(
        BasicASTInternal("SS", List(
            BasicASTInternal("AS", List(
                BasicASTInternal("|", List(BasicASTLeaf("|"))),
                BasicASTInternal("FS", List(
                    BasicASTInternal("w", List(BasicASTLeaf("F"))))))),
            BasicASTInternal("⇒", List(BasicASTLeaf("⇒"))),
            BasicASTInternal("FS", List(
                BasicASTInternal("w", List(BasicASTLeaf("F")))))))),
      Some(
        SequentSchema(
          SingleFormulaSchema(
            ArbitraryFormulaSchema("F")),
          Some(ArbitraryFormulaSchema("F"))))))
}