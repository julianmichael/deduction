package propositional.schema

import parsing._
import propositional._
import propositional.schema._

class ConnectiveSchemaTestSuite extends ParsableTestSuite[ConnectiveSchema] {
  val parsable = ConnectiveSchema
  val parameters = ConnectiveSchemaTestParameters
}

object ConnectiveSchemaTestParameters extends ParsableTestParameters[ConnectiveSchema] {
  val children = Set[Parsable[_]](
      Connective,
      Word) ++ 
      ConnectiveTestParameters.children
  val nonterminals: Set[Parsable[_]] = Set(ConnectiveSchema) ++ ConnectiveTestParameters.nonterminals
  val tokens = ConnectiveTestParameters.tokens
  val productions = Set[Production[Parsable[_]]](
    Production(ConnectiveSchema, List(Word)),
    Production(ConnectiveSchema, List(Connective))) ++
    ConnectiveTestParameters.productions
  val cnfProductions = Set[CNFProduction[Parsable[_]]](
    Unary(NormalTag(ConnectiveSchema), NormalTag(Word)),
    Unary(NormalTag(ConnectiveSchema), NormalTag(Connective))) ++
    ConnectiveTestParameters.cnfProductions
  val testParses = Nil
}