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
  val nonterminals = Set("CS") ++ ConnectiveTestParameters.nonterminals
  val tokens = ConnectiveTestParameters.tokens
  val productions = Set[Production](
    RawProduction("CS", List("w")),
    RawProduction("CS", List("C"))) ++
    ConnectiveTestParameters.productions
  val cnfProductions = Set[CNFProduction](
    Unary("CS", "w"),
    Unary("CS", "C")) ++
    ConnectiveTestParameters.cnfProductions
  val testParses = Nil
}