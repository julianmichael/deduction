package propositional

import parsing._
import propositional._

class FormulaTestSuite extends ParsableTestSuite[Formula] {
  val parsable = Formula
  val parameters = FormulaTestParameters
}

object FormulaTestParameters extends ParsableTestParameters[Formula] {
  val children = Set[Parsable[_]](
    Connective,
    Word,
    Terminal("¬"),
    Terminal("("),
    Terminal(")")) ++
    ConnectiveTestParameters.children
  val nonterminals = Set("F") ++ ConnectiveTestParameters.nonterminals
  val tokens = Set("¬", "(", ")") ++ ConnectiveTestParameters.tokens
  val productions = Set[Production](
    RawProduction("F", List("w")),
    RawProduction("F", List("¬", "F")),
    RawProduction("F", List("(", "F", "C", "F", ")"))) ++
    ConnectiveTestParameters.productions
  val cnfProductions = Set[CNFProduction](
    Unary("F", "w"),
    Binary("F", "¬", "F"),
    Binary("F", "(", "{F+C+F+)}"),
    ChunkedBinary("{F+C+F+)}", "F", "{C+F+)}"),
    ChunkedBinary("{C+F+)}", "C", "{F+)}"),
    ChunkedBinary("{F+)}", "F", ")")) ++
    ConnectiveTestParameters.cnfProductions
  val testParses = List(
    TestParse[Formula](
      Some("p"),
      Some(List("p")),
      None,
      Some(Atom("p"))),
    TestParse[Formula](
      Some("¬p"),
      Some(List("¬", "p")),
      None,
      Some(Negation(Atom("p")))),
    TestParse[Formula](
      Some("(p∧q)"),
      Some(List("(", "p", "∧", "q", ")")),
      None,
      Some(Compound(And, Atom("p"), Atom("q")))),
    TestParse[Formula](
      Some("(p∨q)"),
      Some(List("(", "p", "∨", "q", ")")),
      None,
      Some(Compound(Or, Atom("p"), Atom("q")))),
    TestParse[Formula](
      Some("(p→q)"),
      Some(List("(", "p", "→", "q", ")")),
      None,
      Some(Compound(Implies, Atom("p"), Atom("q")))),
    TestParse[Formula](
      Some("¬(p→q)"),
      Some(List("¬", "(", "p", "→", "q", ")")),
      None,
      Some(Negation(Compound(Implies, Atom("p"), Atom("q"))))),
    TestParse[Formula](
      Some("( One ∧ Two )"),
      Some(List("(", "One", "∧", "Two", ")")),
      None,
      Some(Compound(And, Atom("One"), Atom("Two")))),
    TestParse[Formula](
      Some("(ONE ∨ TWO)"),
      Some(List("(", "ONE", "∨", "TWO", ")")),
      None,
      Some(Compound(Or, Atom("ONE"), Atom("TWO")))),
    TestParse[Formula](
      Some("¬(¬(F ∧ G) ∨ (p → ¬q))"),
      Some(List("¬", "(", "¬", "(", "F", "∧", "G", ")",
        "∨", "(", "p", "→", "¬", "q", ")", ")")),
      None,
      Some(Negation(Compound(Or,
        Negation(Compound(And, Atom("F"), Atom("G"))),
        Compound(Implies, Atom("p"), Negation(Atom("q"))))))),
    TestParse[Formula](
      Some("(p ∧ ¬ q)"),
      Some(List("(", "p", "∧", "¬", "q", ")")),
      None,
      Some(Compound(And, Atom("p"), Negation(Atom("q"))))))
}