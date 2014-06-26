package propositional

import parsing._
import propositional._

class AssumptionsTestSuite extends ParsableTestSuite[Assumptions] {
  val parsable = Assumptions
  val parameters = AssumptionsTestParameters
}

object AssumptionsTestParameters extends ParsableTestParameters[Assumptions] {
  val children = Set[Parsable[_]](
    Formula,
    Word,
    Terminal(",")) ++
    FormulaTestParameters.children ++
    ConnectiveTestParameters.children
  val nonterminals = Set("A") ++ FormulaTestParameters.nonterminals
  val tokens = Set(",") ++ FormulaTestParameters.tokens
  val productions = Set[Production](
    RawProduction("A", List("F")),
    RawProduction("A", List("A", ",", "F"))) ++
    FormulaTestParameters.productions
  val cnfProductions = Set[CNFProduction](
    Unary("A", "F"),
    Binary("A", "A", "{,+F}"),
    ChunkedBinary("{,+F}", ",", "F")) ++
    FormulaTestParameters.cnfProductions
  val testParses = Nil
}