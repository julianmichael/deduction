package propositional.schema

import parsing._
import propositional._
import propositional.schema._

object ConnectiveSchemaTestParameters extends ParsableTestParameters[ConnectiveSchema] {
  val children = Set[Parsable[_]](
      Connective,
      Word) ++ 
      ConnectiveTestParameters.children
  val nonterminals = Set("CS") ++ ConnectiveTestParameters.nonterminals
  val terminals = ConnectiveTestParameters.terminals
  val productions = Set[Production](
    RawProduction("CS", List("w")),
    RawProduction("CS", List("C"))) ++
    ConnectiveTestParameters.productions
  val cnfProductions = Set[CNFProduction](
    Unary("CS", "w"),
    Unary("CS", "C")) ++
    ConnectiveTestParameters.cnfProductions
  val goodStrings = Nil
  val goodTokenizations = Nil
  val goodASTs = Nil
  val goodSymbolics = Nil
}