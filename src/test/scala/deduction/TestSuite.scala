package deduction

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestSuite extends FunSuite {

  val connectiveProductions = Set[Production](
    RawProduction("C", List("∧")),
    RawProduction("C", List("∨")),
    RawProduction("C", List("→")))
  val connectiveCNFProductions = Set[CNFProduction](
    Unary("C", "∧"),
    Unary("C", "∨"),
    Unary("C", "→"))

  val formulaProductions = Set[Production](
    RawProduction("F", List("a")),
    RawProduction("F", List("¬", "F")),
    RawProduction("F", List("(", "F", "C", "F", ")")))
  val formulaCNFProductions = Set[CNFProduction](
    Unary("F", "a"),
    Binary("F", "¬", "F"),
    Binary("F", "(", "{F+C+F+)}"),
    ChunkedBinary("{F+C+F+)}", "F", "{C+F+)}"),
    ChunkedBinary("{C+F+)}", "C", "{F+)}"),
    ChunkedBinary("{F+)}", "F", ")"))

  test("Formula grammar productions") {
    assert(Formula.grammar.productions === (connectiveProductions ++ formulaProductions))
  }

  test("Formula CNF productions") {
    assert(Formula.grammar.cnfProductions === (connectiveCNFProductions ++ formulaCNFProductions))
  }

  test("Formula nonterminals") {
    assert(Formula.grammar.nonterminals === Set("C", "F"))
  }

  test("Formula terminals") {
    assert(Formula.grammar.terminals === Set("¬", "(", "∧", "∨", "→", ")"))
  }

  val goodFormulaStrings: List[String] = List(
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
  val goodFormulaTokenizations: List[List[String]] = List(
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

  goodFormulaStrings.zip(goodFormulaTokenizations).foreach {
    case (string, toks) => test(s"$string tokenizing") {
      assert(Formula.grammar.tokenize(string) === toks)
    }
  }

  val goodFormulaASTs = goodFormulaStrings.flatMap(Formula.grammar.parse)
  // testing these directly would be ridiculous; instead:

  def testASTForGrammar(prods: Set[Production])(ast: AST) =
    test(s"AST $ast has properly formed productions") {
      ast.children match {
        case Nil => assert(ast.production === None)
        case xs => {
          ast.production match {
            case None    => assert(false)
            case Some(p) => assert(prods(p))
          }
        }
      }
    }
  goodFormulaASTs.foreach(testASTForGrammar(formulaProductions))

  val symbolicFormulas = goodFormulaASTs.map(Formula.fromAST)
  val goodFormulas = List(
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

  (goodFormulaStrings zip (goodFormulaASTs zip symbolicFormulas)).foreach {
    case (string, (ast, formula)) => {
      test(s"Symbolic representation of $string") {
        assert(Formula.fromAST(ast) === formula)
      }
    }
  }

  val assumptionProductions = Set[Production](
    RawProduction("A", List("F")),
    RawProduction("A", List("A", ",", "F")))
  val assumptionCNFProductions = Set[CNFProduction](
    Unary("A", "F"),
    Binary("A", "A", "{,+F}"),
    ChunkedBinary("{,+F}", ",", "F"))

  val sequentProductions = Set[Production](
    RawProduction("S", List("A", "⇒", "F")),
    RawProduction("S", List("⇒", "F")),
    RawProduction("S", List("A", "⇒")))
  val sequentCNFProductions = Set[CNFProduction](
      Binary("S", "A", "{⇒+F}"),
      ChunkedBinary("{⇒+F}", "⇒", "F"),
      Binary("S", "⇒", "F"),
      Binary("S", "A", "⇒")
  )
  
  test("Sequent grammar productions") {
    assert(Sequent.grammar.productions === (
        connectiveProductions ++
        formulaProductions ++
        assumptionProductions ++ 
        sequentProductions))
  }
  
  test("Sequent CNF productions") {
    assert(Sequent.grammar.cnfProductions === (
        connectiveCNFProductions ++
        formulaCNFProductions ++
        assumptionCNFProductions ++ 
        sequentCNFProductions))
  }
  
  test("Sequent nonterminals") {
    assert(Sequent.grammar.nonterminals === Set("S", "A", "F", "C"))
  }
  
  test("Sequent terminals") {
    assert(Sequent.grammar.terminals === Set("¬", "(", "∧", "∨", "→", ")", ",", "⇒"))
  }

  val goodSequentStrings = List(
    "p ⇒ p",
    "F ⇒ F",
    "⇒ (p ∨ ¬p)",
    "⇒ (F ∨ ¬F)",
    "F ⇒",
    "F, ¬F ⇒",
    "F, G ⇒ (F ∧ G)")

  val goodSequentASTs = goodSequentStrings.flatMap(Sequent.grammar.parse)
  goodSequentASTs.foreach(testASTForGrammar(sequentProductions))

  val goodSequents = List(
      Sequent(Assumptions(Set(Atom("p"))), Some(Atom("p"))),
      Sequent(Assumptions(Set(Atom("F"))), Some(Atom("F"))),
      Sequent(Assumptions(Set()), Some(Compound(Or, Atom("p"), Negation(Atom("p"))))),
      Sequent(Assumptions(Set()), Some(Compound(Or, Atom("F"), Negation(Atom("F"))))),
      Sequent(Assumptions(Set(Atom("F"))), None),
      Sequent(Assumptions(Set(Atom("F"), Negation(Atom("F")))), None),
      Sequent(Assumptions(Set(Atom("F"), Atom("G"))), Some(Compound(And, Atom("F"), Atom("G")))))
  
  val symbolicSequents = goodSequentASTs.map(Sequent.fromAST)
  
  (goodSequentStrings zip (goodSequentASTs zip symbolicSequents)).foreach {
    case (string, (ast, sequent)) => {
      test(s"Symbolic representation of $string") {
        assert(Sequent.fromAST(ast) === sequent)
      }
    }
  }
  
  /*
  val orSchema =
    CompoundSchema(
      SpecifiedConnectiveSchema(Or),
      CompoundSchema(
        SpecifiedConnectiveSchema(Or),
        ArbitraryFormulaSchema("F"),
        ArbitraryFormulaSchema("G")),
      ArbitraryFormulaSchema("F"))
  val orFormulas = List(
    "((p ∨ q) ∨ p)", // yes
    "((¬p ∨ q) ∨ ¬p)", // yes
    "((p ∨ q) ∨ q)", // no
    "((p ∧ q) ∨ p)", // no
    "(((p ∧ q) ∨ q) ∨ (p ∧ q))", // yes
    "((¬(p ∧ q) ∨ q) ∨ ¬(p ∧ q))" // yes
    )
  orFormulas.map(Formula.fromString).map(_.flatMap(x => Some(orSchema.matches(x)))).map(println)

  println("----------")
  val axiom1 =
    SequentSchema(
      SingleFormulaSchema(
        ArbitraryFormulaSchema("F")),
      Some(ArbitraryFormulaSchema("F")))

  val axiom1Sequents = List(
    "F ⇒ F", // yes
    "p ⇒ p", // yes
    "(p ∧ p) ⇒ (p ∧ p)", // yes
    "((¬(p ∧ q) ∨ q) ∨ ¬(p ∧ q)) ⇒ ((¬(p ∧ q) ∨ q) ∨ ¬(p ∧ q))", // yes
    "(p) ⇒ (p)", // no (None)
    "p ⇒ q", //no (Nil)
    "((¬(p ∧ q) ∨ q) ∨ ¬(p ∧ q)) ⇒ ((¬(p ∧ q) ∨ q) ∨ ¬(p ∧ p))" // no (Nil)
    )
  axiom1Sequents.map(Sequent.fromString).map(_.flatMap(x => Some(axiom1.matches(x)))).map(println)

  println("----------")
  val concludeAnythingAxiom =
    SequentSchema(
      EmptySchema,
      Some(ArbitraryFormulaSchema("F")))
  val concludeAnythingSequents = List(
    "⇒ F", // yes
    "⇒ (p ∧ p)", // yes
    "⇒ ¬(p ∨ q)", // yes
    "A ⇒ p", // no
    "A, B ⇒ q", // no
    "⇒ (p)", // no (None)
    "⇒ p ∧ q" // no (None)
    )
  concludeAnythingSequents.map(Sequent.fromString).map(_.flatMap(x => Some(concludeAnythingAxiom.matches(x)))).map(println)

  println("---------- THE FIRST DEDUCTION ----------")
  val premises = (("p ⇒ p" :: "q ⇒ q" :: Nil) map Sequent.fromString).flatten
  val conjunctionIntroduction = DeductionRule.naturalDeductionRules("∧I")
  println(s"Rule:\t\t$conjunctionIntroduction")
  println(s"Premises:\t${premises.mkString(", ")}")
  val conclusion = Sequent.fromString("p, q ⇒ (p ∧ q)").get
  println(s"Conclusion:\t$conclusion")
  val deduction = Deduction(premises, conclusion)
  println(s"Matching:\t${conjunctionIntroduction.matches(deduction)}")
  */
}