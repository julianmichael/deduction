package propositional

import parsing._
import propositional._

class ConnectiveTestSuite extends ParsableTestSuite[Connective] {
  val parsable = Connective
  val parameters = ConnectiveTestParameters
}

object ConnectiveTestParameters extends ParsableTestParameters[Connective] {
  val children = Set[Parsable[_]](
    Terminal("∧"),
    Terminal("∨"),
    Terminal("→"))
  val nonterminals = Set("C")
  val tokens = Set("∧", "∨", "→")
  val productions = Set[Production](
    RawProduction("C", List("∧")),
    RawProduction("C", List("∨")),
    RawProduction("C", List("→")))
  val cnfProductions = Set[CNFProduction](
    Unary("C", "∧"),
    Unary("C", "∨"),
    Unary("C", "→"))
  val testParses = List(
    TestParse[Connective](
      Some("∧"),
      None,
      Some(BasicASTInternal("C", List(BasicASTInternal("∧", List(BasicASTLeaf("∧")))))),
      Some(And)),
    TestParse[Connective](
      Some("∨"),
      None,
      Some(BasicASTInternal("C", List(BasicASTInternal("∨", List(BasicASTLeaf("∨")))))),
      Some(Or)),
    TestParse[Connective](
      Some("→"),
      None,
      Some(BasicASTInternal("C", List(BasicASTInternal("→", List(BasicASTLeaf("→")))))),
      Some(Implies)))
}