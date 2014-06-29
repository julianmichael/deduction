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
    Name,
    Terminal("("),
    Terminal(")"),
    Terminal("¬")) ++
    ConnectiveSchemaTestParameters.children
  val nonterminals: Set[Parsable[_]] =
    Set(FormulaSchema) ++
      ConnectiveSchemaTestParameters.nonterminals
  val tokens =
    Set("(", ")", "¬") ++
      ConnectiveSchemaTestParameters.tokens
  val productions = Set[Production[Parsable[_]]](
    Production(FormulaSchema, List(Name)),
    Production(FormulaSchema, List(Terminal("¬"), FormulaSchema)),
    Production(FormulaSchema, List(Terminal("("), FormulaSchema, ConnectiveSchema, FormulaSchema, Terminal(")")))) ++
    ConnectiveSchemaTestParameters.productions
  val cnfProductions = Set[CNFProduction[Parsable[_]]](
    Unary(NormalTag(FormulaSchema), NormalTag(Name)),
    Binary(NormalTag(FormulaSchema), NormalTag(Terminal("¬")), NormalTag(FormulaSchema)),
    Binary(NormalTag(FormulaSchema), NormalTag(Terminal("(")), ChunkedTag(List(FormulaSchema, ConnectiveSchema, FormulaSchema, Terminal(")")))),
    Binary(ChunkedTag(List(FormulaSchema, ConnectiveSchema, FormulaSchema, Terminal(")"))), NormalTag(FormulaSchema), ChunkedTag(List(ConnectiveSchema, FormulaSchema, Terminal(")")))),
    Binary(ChunkedTag(List(ConnectiveSchema, FormulaSchema, Terminal(")"))), NormalTag(ConnectiveSchema), ChunkedTag(List(FormulaSchema, Terminal(")")))),
    Binary(ChunkedTag(List(FormulaSchema, Terminal(")"))), NormalTag(FormulaSchema), NormalTag(Terminal(")")))) ++
    ConnectiveSchemaTestParameters.cnfProductions
  val testParses = List(
    TestParse[FormulaSchema](
      Some("((F ∨ G) ∨ F)"),
      None,
      None, 
      Some(CompoundSchema(
        SpecifiedConnectiveSchema(Or),
        CompoundSchema(
          SpecifiedConnectiveSchema(Or),
          ArbitraryFormulaSchema("F"),
          ArbitraryFormulaSchema("G")),
        ArbitraryFormulaSchema("F")))))
}