package deduction

object TestMain extends App {

  val goodOrFormulaStrings = List(
    "((p ∨ q) ∨ p)", // yes
    "((¬p ∨ q) ∨ ¬p)", // yes
    "(((p ∧ q) ∨ q) ∨ (p ∧ q))", // yes
    "((¬(p ∧ q) ∨ q) ∨ ¬(p ∧ q))" // yes
    )
  val badOrFormulaStrings = List(
    "((p ∨ q) ∨ q)", // no
    "((p ∧ q) ∨ p)" // no
    )
  val goodOrFormulas = goodOrFormulaStrings.map(Formula.fromString)
  goodOrFormulas map (_ map FormulaSchemaTestParameters.goodSymbolics.head.matches) map println

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
  println("----------")
  val ruleFromString = for {
    a1 <- SequentSchema.fromString("Γ ⇒ F")
    a2 <- SequentSchema.fromString("Δ ⇒ G")
    conc <- SequentSchema.fromString("Γ ∪ Δ ⇒ (F ∧ G)")
  } yield DeductionRule(List(a1, a1), conc)
  println(conjunctionIntroduction)
  println(ruleFromString)
  println(Some(conjunctionIntroduction) == ruleFromString)
  
  println(SequentSchema.grammar.parsings("Γ ⇒ F"))
}