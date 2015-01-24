package deduction

import propositional.Formula
import propositional.Sequent
import propositional.schema.EmptySchema
import propositional.schema.ArbitraryFormulaSchema
import propositional.schema.SequentSchema
import propositional.schema.FormulaSchemaTestParameters
import propositional.schema.SequentSchemaTestParameters

import scalaz._
import Scalaz._

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
  goodOrFormulas map (_ map FormulaSchemaTestParameters.testParses.head.symbolic.get.matches) map println

  val axiom1 = SequentSchemaTestParameters.testParses.head.symbolic.get
  val axiom1FromString = SequentSchema.fromStringUnique("|F ⇒ F").get
  println("-----")
  println(axiom1)
  println(axiom1FromString)
  println("-----")
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
  val conclusion = Sequent.fromStringUnique("p, q ⇒ (p ∧ q)").get
  println(s"Conclusion:\t$conclusion")
  val deduction = Deduction(premises, conclusion)
  println(s"Matching:\t${conjunctionIntroduction.matches(deduction)}")
  println("----------")
  val ruleFromString = (for {
    a1 <- SequentSchema.fromString("Γ ⇒ F")
    a2 <- SequentSchema.fromString("Δ ⇒ G")
    conc <- SequentSchema.fromString("Γ ∪ Δ ⇒ (F ∧ G)")
  } yield DeductionRule(List(a1, a2), conc)).headOption
  println(ruleFromString)
  println(Some(conjunctionIntroduction) == ruleFromString)

  // From Martin Odersky on SO
  /** Returns shortest possible list of lists xss such that
  *   - xss.flatten == xs
  *   - No sublist in xss contains an element matching p in its tail
  */
 def groupPrefix[T](xs: List[T])(p: T => Boolean): List[List[T]] = xs match {
   case List() => List()
   case x :: xs1 => 
     val (ys, zs) = xs1 span (!p(_))
     (x :: ys) :: groupPrefix(zs)(p)
 }

  println("--------------------")
  def validateProof(proofLines: List[String]) = {
    val allProofsLines = groupPrefix(proofLines)(_.startsWith("=="))
    val allProofSeqLists = allProofsLines map (_.flatMap(Sequent.fromString).toList)
    val proofObjects = allProofSeqLists map { proofSeqList =>
      val res = new DeductionProof[Option](proofSeqList.reverse.head, proofSeqList)
      println(res.prettyString)
      res
    }
  }
  import scala.io.Source
  val proofFromFile = Source.fromFile("proofs/wem.txt").getLines.toList
  validateProof(proofFromFile)
  println("--------------------")
}