package propositional

import parsing._
import propositional._

class SequentTestSuite extends ParsableTestSuite[Sequent] {
  val parsable = Sequent
  val parameters = SequentTestParameters 
}

object SequentTestParameters extends ParsableTestParameters[Sequent] {
  val children = Set[Parsable[_]](
    Assumptions,
    Formula,
    Word,
    Terminal("⇒")) ++
    AssumptionsTestParameters.children ++
    FormulaTestParameters.children
  val nonterminals: Set[Parsable[_]] = Set(Sequent) ++ AssumptionsTestParameters.nonterminals ++ FormulaTestParameters.nonterminals
  val tokens = Set("⇒") ++ AssumptionsTestParameters.tokens ++ FormulaTestParameters.tokens
  val productions = Set[Production[Parsable[_]]](
    Production(Sequent, List(Assumptions, Terminal("⇒"), Formula)),
    Production(Sequent, List(Terminal("⇒"), Formula)),
    Production(Sequent, List(Assumptions, Terminal("⇒")))) ++
    AssumptionsTestParameters.productions ++
    FormulaTestParameters.productions
  val cnfProductions = Set[CNFProduction[Parsable[_]]](
    Binary(NormalTag(Sequent), NormalTag(Assumptions), ChunkedTag(List(Terminal("⇒"), Formula))),
    Binary(ChunkedTag(List(Terminal("⇒"), Formula)), NormalTag(Terminal("⇒")), NormalTag(Formula)),
    Binary(NormalTag(Sequent), NormalTag(Terminal("⇒")), NormalTag(Formula)),
    Binary(NormalTag(Sequent), NormalTag(Assumptions), NormalTag(Terminal("⇒")))) ++
    AssumptionsTestParameters.cnfProductions ++
    FormulaTestParameters.cnfProductions
  val testParses = List(
    TestParse[Sequent](
      Some("p ⇒ p"),
      None,
      None,
      Some(Sequent(Assumptions(Set(Atom("p"))), Some(Atom("p"))))),
    TestParse[Sequent](
      Some("F ⇒ F"),
      None,
      None,
      Some(Sequent(Assumptions(Set(Atom("F"))), Some(Atom("F"))))),
    TestParse[Sequent](
      Some("⇒ (p ∨ ¬p)"),
      None,
      None,
      Some(Sequent(Assumptions(Set()), Some(Compound(Or, Atom("p"), Negation(Atom("p"))))))),
    TestParse[Sequent](
      Some("⇒ (F ∨ ¬F)"),
      None,
      None,
      Some(Sequent(Assumptions(Set()), Some(Compound(Or, Atom("F"), Negation(Atom("F"))))))),
    TestParse[Sequent](
      Some("F ⇒"),
      None,
      None,
      Some(Sequent(Assumptions(Set(Atom("F"))), None))),
    TestParse[Sequent](
      Some("F, ¬F ⇒"),
      None,
      None,
      Some(Sequent(Assumptions(Set(Atom("F"), Negation(Atom("F")))), None))),
    TestParse[Sequent](
      Some("F, G ⇒ (F ∧ G)"),
      None,
      None,
      Some(Sequent(Assumptions(Set(Atom("F"), Atom("G"))),
        Some(Compound(And, Atom("F"), Atom("G")))))))
}