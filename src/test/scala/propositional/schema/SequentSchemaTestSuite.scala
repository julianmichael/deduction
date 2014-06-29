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
  val nonterminals: Set[Parsable[_]] =
    Set(SequentSchema) ++
      AssumptionSchemaTestParameters.nonterminals ++
      FormulaSchemaTestParameters.nonterminals
  val tokens =
    Set("⇒") ++
      AssumptionSchemaTestParameters.tokens ++
      FormulaSchemaTestParameters.tokens
  val productions = Set[Production[Parsable[_]]](
    Production(SequentSchema, List(AssumptionSchema, Terminal("⇒"), FormulaSchema)),
    Production(SequentSchema, List(AssumptionSchema, Terminal("⇒"))),
    Production(SequentSchema, List(Terminal("⇒"), FormulaSchema))) ++
    AssumptionSchemaTestParameters.productions ++
    FormulaSchemaTestParameters.productions
  val cnfProductions = Set[CNFProduction[Parsable[_]]](
    Binary(NormalTag(SequentSchema), NormalTag(AssumptionSchema), ChunkedTag(List(Terminal("⇒"), FormulaSchema))),
    Binary(ChunkedTag(List(Terminal("⇒"), FormulaSchema)), NormalTag(Terminal("⇒")), NormalTag(FormulaSchema)),
    Binary(NormalTag(SequentSchema), NormalTag(AssumptionSchema), NormalTag(Terminal("⇒"))),
    Binary(NormalTag(SequentSchema), NormalTag(Terminal("⇒")), NormalTag(FormulaSchema))) ++
    AssumptionSchemaTestParameters.cnfProductions ++
    FormulaSchemaTestParameters.cnfProductions
  val testParses = List(
    TestParse[SequentSchema](
      Some("|F ⇒ F"),
      Some(List("|", "F", "⇒", "F")),
      Some(
        ASTParent[Parsable[_]](SequentSchema, List(
            ASTParent[Parsable[_]](AssumptionSchema, List(
                ASTParent(Terminal("|"), List(ASTLeaf("|"))),
                ASTParent(FormulaSchema, List(
                    ASTParent(Word, List(ASTLeaf("F"))))))),
            ASTParent(Terminal("⇒"), List(ASTLeaf("⇒"))),
            ASTParent(FormulaSchema, List(
                ASTParent(Word, List(ASTLeaf("F")))))))),
      Some(
        SequentSchema(
          SingleFormulaSchema(
            ArbitraryFormulaSchema("F")),
          Some(ArbitraryFormulaSchema("F"))))))
}