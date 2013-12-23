package deduction

object SequentTestParameters extends ParsableTestParameters[Sequent] {
  val nonterminals = Set("S") ++ AssumptionsTestParameters.nonterminals ++ FormulaTestParameters.nonterminals
  val terminals = Set("⇒") ++ AssumptionsTestParameters.terminals ++ FormulaTestParameters.terminals
  val productions = Set[Production](
    RawProduction("S", List("A", "⇒", "F")),
    RawProduction("S", List("⇒", "F")),
    RawProduction("S", List("A", "⇒"))) ++
    AssumptionsTestParameters.productions ++
    FormulaTestParameters.productions
  val cnfProductions = Set[CNFProduction](
    Binary("S", "A", "{⇒+F}"),
    ChunkedBinary("{⇒+F}", "⇒", "F"),
    Binary("S", "⇒", "F"),
    Binary("S", "A", "⇒")) ++
    AssumptionsTestParameters.cnfProductions ++
    FormulaTestParameters.cnfProductions
  val goodStrings = List(
    "p ⇒ p",
    "F ⇒ F",
    "⇒ (p ∨ ¬p)",
    "⇒ (F ∨ ¬F)",
    "F ⇒",
    "F, ¬F ⇒",
    "F, G ⇒ (F ∧ G)")
  val goodTokenizations = Nil
  val goodASTs = Nil
  val goodSymbolics = List(
    Sequent(Assumptions(Set(Atom("p"))), Some(Atom("p"))),
    Sequent(Assumptions(Set(Atom("F"))), Some(Atom("F"))),
    Sequent(Assumptions(Set()), Some(Compound(Or, Atom("p"), Negation(Atom("p"))))),
    Sequent(Assumptions(Set()), Some(Compound(Or, Atom("F"), Negation(Atom("F"))))),
    Sequent(Assumptions(Set(Atom("F"))), None),
    Sequent(Assumptions(Set(Atom("F"), Negation(Atom("F")))), None),
    Sequent(Assumptions(Set(Atom("F"), Atom("G"))), Some(Compound(And, Atom("F"), Atom("G")))))
}