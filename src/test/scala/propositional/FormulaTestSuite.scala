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
    AtomicFormula,
    Terminal("¬"),
    Terminal("("),
    Terminal(")")) ++
    ConnectiveTestParameters.children
  val nonterminals: Set[Parsable[_]] = Set(Formula) ++ ConnectiveTestParameters.nonterminals
  val tokens = Set("¬", "(", ")") ++ ConnectiveTestParameters.tokens
  val productions = Set[Production[Parsable[_]]](
    Production(Formula, List(AtomicFormula)),
    Production(Formula, List(Terminal("¬"), Formula)),
    Production(Formula, List(Terminal("("), Formula, Connective, Formula, Terminal(")")))) ++
    ConnectiveTestParameters.productions
  val cnfProductions = Set[CNFProduction[Parsable[_]]](
    Unary(NormalTag(Formula), NormalTag(AtomicFormula)),
    Binary(NormalTag(Formula), NormalTag(Terminal("¬")), NormalTag(Formula)),
    Binary(NormalTag(Formula), NormalTag(Terminal("(")), ChunkedTag(List(Formula, Connective, Formula, Terminal(")")))),
    Binary(ChunkedTag(List(Formula, Connective, Formula, Terminal(")"))), NormalTag(Formula), ChunkedTag(List(Connective, Formula, Terminal(")")))),
    Binary(ChunkedTag(List(Connective, Formula, Terminal(")"))), NormalTag(Connective), ChunkedTag(List(Formula, Terminal(")")))),
    Binary(ChunkedTag(List(Formula, Terminal(")"))), NormalTag(Formula), NormalTag(Terminal(")")))) ++
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