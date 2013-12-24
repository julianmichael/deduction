package propositional

import parsing._
import propositional._

object FormulaTestParameters extends ParsableTestParameters[Formula] {
  val children = Set[Parsable[_]](
      Connective,
      Word,
      Terminal("¬"),
      Terminal("("),
      Terminal(")")) ++
      ConnectiveTestParameters.children
  val nonterminals = Set("F") ++ ConnectiveTestParameters.nonterminals
  val terminals = Set("¬", "(", ")") ++ ConnectiveTestParameters.terminals
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
  val goodStrings: List[String] = List(
    "p",
    "¬p",
    "(p∧q)",
    "(p∨q)",
    "(p→q)",
    "¬(p→q)",
    "( One ∧ Two )",
    "(ONE ∨ TWO)",
    "¬(¬(F ∧ G) ∨ (p → ¬q))",
    "(p ∧ ¬ q)")
  val goodTokenizations: List[List[String]] = List(
    List("p"),
    List("¬", "p"),
    List("(", "p", "∧", "q", ")"),
    List("(", "p", "∨", "q", ")"),
    List("(", "p", "→", "q", ")"),
    List("¬", "(", "p", "→", "q", ")"),
    List("(", "One", "∧", "Two", ")"),
    List("(", "ONE", "∨", "TWO", ")"),
    List("¬", "(", "¬", "(", "F", "∧", "G", ")", "∨", "(", "p", "→", "¬", "q", ")", ")"),
    List("(", "p", "∧", "¬", "q", ")"))

  // nope, skipping this part
  val goodASTs = Nil

  val goodSymbolics = List(
    Atom("p"),
    Negation(Atom("p")),
    Compound(And, Atom("p"), Atom("q")),
    Compound(Or, Atom("p"), Atom("q")),
    Compound(Implies, Atom("p"), Atom("q")),
    Negation(Compound(Implies, Atom("p"), Atom("q"))),
    Compound(And, Atom("One"), Atom("Two")),
    Compound(Or, Atom("ONE"), Atom("TWO")),
    Negation(Compound(Or, Negation(Compound(And, Atom("F"), Atom("G"))), Compound(Implies, Atom("p"), Negation(Atom("q"))))),
    Compound(And, Atom("p"), Negation(Atom("q"))))
}