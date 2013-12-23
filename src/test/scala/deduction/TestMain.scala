package deduction

object TestMain extends App {
  
  
//  val formulas = orFormulas.flatMap(Formula.fromString)
//  formulas.foreach(println)
//  println("----------")
//  val namings = formulas.flatMap(orSchema.matches)
//  namings.foreach(println)
//  println("----------")
//  orFormulas.map(Formula.fromString).map(_.flatMap(x => Some(orSchema.matches(x)))).map(println)

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
}