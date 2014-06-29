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
    Terminal(",")) ++
    FormulaTestParameters.children ++
    ConnectiveTestParameters.children
  val nonterminals: Set[Parsable[_]] = Set(Assumptions) ++ FormulaTestParameters.nonterminals
  val tokens = Set(",") ++ FormulaTestParameters.tokens
  val productions = Set[Production[Parsable[_]]](
    Production[Parsable[_]](Assumptions, List(Formula)),
    Production[Parsable[_]](Assumptions, List(Assumptions, Terminal(","), Formula))) ++
    FormulaTestParameters.productions
  val cnfProductions = Set[CNFProduction[Parsable[_]]](
    Unary(NormalTag[Parsable[_]](Assumptions), NormalTag[Parsable[_]](Formula)),
    Binary(NormalTag[Parsable[_]](Assumptions), NormalTag[Parsable[_]](Assumptions), ChunkedTag[Parsable[_]](List(Terminal(","), Formula))),
    Binary(ChunkedTag[Parsable[_]](List(Terminal(","), Formula)), NormalTag[Parsable[_]](Terminal(",")), NormalTag[Parsable[_]](Formula))) ++
    FormulaTestParameters.cnfProductions
  val testParses = Nil
}