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
  val nonterminals: Set[Parsable[_]] = Set(Connective)
  val tokens = Set("∧", "∨", "→")
  val productions = Set[Production[Parsable[_]]](
    Production(Connective, List(Terminal("∧"))),
    Production(Connective, List(Terminal("∨"))),
    Production(Connective, List(Terminal("→"))))
  val cnfProductions = Set[CNFProduction[Parsable[_]]](
    Unary(NormalTag[Parsable[_]](Connective), NormalTag[Parsable[_]](Terminal("∧"))),
    Unary(NormalTag[Parsable[_]](Connective), NormalTag[Parsable[_]](Terminal("∨"))),
    Unary(NormalTag[Parsable[_]](Connective), NormalTag[Parsable[_]](Terminal("→"))))
  val testParses = List(
    TestParse[Connective](
      Some("∧"),
      None,
      Some(ASTParent[Parsable[_]](Connective, List(ASTParent[Parsable[_]](Terminal("∧"), List(ASTLeaf("∧")))))),
      Some(And)),
    TestParse[Connective](
      Some("∨"),
      None,
      Some(ASTParent[Parsable[_]](Connective, List(ASTParent[Parsable[_]](Terminal("∨"), List(ASTLeaf("∨")))))),
      Some(Or)),
    TestParse[Connective](
      Some("→"),
      None,
      Some(ASTParent[Parsable[_]](Connective, List(ASTParent[Parsable[_]](Terminal("→"), List(ASTLeaf("→")))))),
      Some(Implies)))
}