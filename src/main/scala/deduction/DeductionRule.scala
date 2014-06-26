package deduction

import propositional._
import propositional.schema._

abstract class SequentValidation {
  def name: String
}
case class ValidatedAxiom(
    axiom: Sequent,
    schema: SequentSchema,
    val name: String) extends SequentValidation
case class ValidatedDeduction(
    deduction: Deduction,
    schema: DeductionRule,
    val name: String) extends SequentValidation

case class Deduction(premises: List[Sequent], conclusion: Sequent)

case class DeductionRule(
  premiseSchemas: List[SequentSchema],
  conclusionSchema: SequentSchema) extends Schema[Deduction] {
  def matches(targ: Deduction) = targ match {
    case Deduction(premises, conclusion) => {
      if (premises.length != premiseSchemas.length) {
        Nil
      } else {
        val premiseNamingsList = (premiseSchemas, premises).zipped.map(_.matches(_))
        val conclusionNamings = conclusionSchema.matches(conclusion)
        consistentNamings(conclusionNamings :: premiseNamingsList)
      }
    }
  }

  override val toString = {
    val premiseString = premiseSchemas match {
      case Nil  => ""
      case list => list.mkString(", ")
    }
    val conclusionString = conclusionSchema.toString
    s"$premiseString | $conclusionString"
  }
}
object DeductionRule {
  val naturalDeductionAxiomSchemas = Map[String, SequentSchema](
    "Identity" -> (for {
      ax <- SequentSchema.fromStringUnique("|F ⇒ F")
    } yield ax).get,
    "Excluded Middle" -> (for {
      ax <- SequentSchema.fromStringUnique("⇒ (F ∨ ¬F)")
    } yield ax).get)
  val naturalDeductionRules = Map[String, DeductionRule](
    "∧I" -> (for {
      p1 <- SequentSchema.fromStringUnique("Γ ⇒ F")
      p2 <- SequentSchema.fromStringUnique("Δ ⇒ G")
      conc <- SequentSchema.fromStringUnique("Γ ∪ Δ ⇒ (F ∧ G)")
    } yield DeductionRule(List(p1, p2), conc)).get,
    "∨I1" -> (for {
      p <- SequentSchema.fromStringUnique("Γ ⇒ F")
      conc <- SequentSchema.fromStringUnique("Γ ⇒ (F ∨ G)")
    } yield DeductionRule(List(p), conc)).get,
    "∨I2" -> (for {
      p <- SequentSchema.fromStringUnique("Γ ⇒ G")
      conc <- SequentSchema.fromStringUnique("Γ ⇒ (F ∨ G)")
    } yield DeductionRule(List(p), conc)).get,
    "→I" -> (for {
      p <- SequentSchema.fromStringUnique("Γ, F ⇒ G")
      conc <- SequentSchema.fromStringUnique("Γ ⇒ (F → G)")
    } yield DeductionRule(List(p), conc)).get,
    "¬I" -> (for {
      p <- SequentSchema.fromStringUnique("Γ, F ⇒")
      conc <- SequentSchema.fromStringUnique("Γ ⇒ ¬F")
    } yield DeductionRule(List(p), conc)).get,
    "∧E1" -> (for {
      p <- SequentSchema.fromStringUnique("Γ ⇒ (F ∧ G)")
      conc <- SequentSchema.fromStringUnique("Γ ⇒ F")
    } yield DeductionRule(List(p), conc)).get,
    "∧E2" -> (for {
      p <- SequentSchema.fromStringUnique("Γ ⇒ (F ∧ G)")
      conc <- SequentSchema.fromStringUnique("Γ ⇒ G")
    } yield DeductionRule(List(p), conc)).get,
    "∨E1" -> (for {
      p1 <- SequentSchema.fromStringUnique("Γ ⇒ (F ∨ G)")
      p2 <- SequentSchema.fromStringUnique("Δ1, F ⇒ Σ")
      p3 <- SequentSchema.fromStringUnique("Δ2, G ⇒ Σ")
      conc <- SequentSchema.fromStringUnique("Γ ∪ Δ1 ∪ Δ2 ⇒ Σ")
    } yield DeductionRule(List(p1, p2, p3), conc)).get,
    "∨E2" -> (for {
      p1 <- SequentSchema.fromStringUnique("Γ ⇒ (F ∨ G)")
      p2 <- SequentSchema.fromStringUnique("Δ1, F ⇒")
      p3 <- SequentSchema.fromStringUnique("Δ2, G ⇒")
      conc <- SequentSchema.fromStringUnique("Γ ∪ Δ1 ∪ Δ2 ⇒")
    } yield DeductionRule(List(p1, p2, p3), conc)).get,
    "→E" -> (for {
      p1 <- SequentSchema.fromStringUnique("Γ ⇒ F")
      p2 <- SequentSchema.fromStringUnique("Δ ⇒ (F → G)")
      conc <- SequentSchema.fromStringUnique("Γ ∪ Δ ⇒ G")
    } yield DeductionRule(List(p1, p2), conc)).get,
    "¬E" -> (for {
      p1 <- SequentSchema.fromStringUnique("Γ ⇒ F")
      p2 <- SequentSchema.fromStringUnique("Δ ⇒ ¬F")
      conc <- SequentSchema.fromStringUnique("Γ ∪ Δ ⇒")
    } yield DeductionRule(List(p1, p2), conc)).get,
    "C" -> (for {
      p <- SequentSchema.fromStringUnique("Γ ⇒")
      conc <- SequentSchema.fromStringUnique("Γ ⇒ F")
    } yield DeductionRule(List(p), conc)).get,
    "W1" -> (for {
      p <- SequentSchema.fromStringUnique("Γ ⇒ Σ")
      conc <- SequentSchema.fromStringUnique("Γ ∪ Δ ⇒ Σ")
    } yield DeductionRule(List(p), conc)).get,
    "W2" -> (for {
      p <- SequentSchema.fromStringUnique("Γ ⇒")
      conc <- SequentSchema.fromStringUnique("Γ ∪ Δ ⇒")
    } yield DeductionRule(List(p), conc)).get)
  val naturalDeductionRules2 = Map[String, DeductionRule](
    "∧I" -> DeductionRule(
      // first premise: Γ ⇒ F
      SequentSchema(
        ArbitrarySetSchema("Γ"),
        Some(ArbitraryFormulaSchema("F")))
        // second premise: Δ ⇒ G
        :: SequentSchema(
          ArbitrarySetSchema("Δ"),
          Some(ArbitraryFormulaSchema("G")))
          :: Nil,
      // conclusion: Γ ∪ Δ ⇒ (F ∧ G)
      SequentSchema(
        UnionSchema(
          ArbitrarySetSchema("Γ"),
          ArbitrarySetSchema("Δ")),
        Some(CompoundSchema(
          SpecifiedConnectiveSchema(And),
          ArbitraryFormulaSchema("F"),
          ArbitraryFormulaSchema("G"))))))
}
