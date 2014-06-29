package propositional.schema

import parsing._
import propositional._
import propositional.schema._

class AssumptionSchemaTestSuite extends ParsableTestSuite[AssumptionSchema] {
  val parsable = AssumptionSchema
  val parameters = AssumptionSchemaTestParameters
}

object AssumptionSchemaTestParameters extends ParsableTestParameters[AssumptionSchema] {
  val children = Set[Parsable[_]](
    FormulaSchema,
    Terminal("∪"),
    Name,
    Terminal(","),
    Terminal("|")) ++
    FormulaSchemaTestParameters.children
  val nonterminals: Set[Parsable[_]] = 
    Set(AssumptionSchema) ++
    FormulaSchemaTestParameters.nonterminals
  val tokens =
    Set(",", "∪", "|") ++
    FormulaSchemaTestParameters.tokens
  val productions = Set[Production[Parsable[_]]](
    Production(AssumptionSchema, List(Name)),
    Production(AssumptionSchema, List(Terminal("|"), FormulaSchema)),
    Production(AssumptionSchema, List(AssumptionSchema, Terminal(","), FormulaSchema)),
    Production(AssumptionSchema, List(AssumptionSchema, Terminal("∪"), AssumptionSchema))) ++
    FormulaSchemaTestParameters.productions
  val cnfProductions = Set[CNFProduction[Parsable[_]]](
    Unary(NormalTag(AssumptionSchema), NormalTag(Name)),
    Binary(NormalTag(AssumptionSchema), NormalTag(Terminal("|")), NormalTag(FormulaSchema)),
    Binary(NormalTag(AssumptionSchema), NormalTag(AssumptionSchema), ChunkedTag(List(Terminal(","), FormulaSchema))),
    Binary(ChunkedTag(List(Terminal(","), FormulaSchema)), NormalTag(Terminal(",")), NormalTag(FormulaSchema)),
    Binary(NormalTag(AssumptionSchema), NormalTag(AssumptionSchema), ChunkedTag(List(Terminal("∪"), AssumptionSchema))),
    Binary(ChunkedTag(List(Terminal("∪"), AssumptionSchema)), NormalTag(Terminal("∪")), NormalTag(AssumptionSchema))) ++
    FormulaSchemaTestParameters.cnfProductions
  val testParses = Nil
}