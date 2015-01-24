package deduction

import propositional.Sequent
import propositional.schema.SequentSchema
import scalaz.MonadPlus
import scalaz._
import Scalaz._

class DeductionProof[M[+_]](
  val goal: Sequent,
  val proof: List[Sequent],
  val axioms: Map[String, SequentSchema] = DeductionRule.naturalDeductionAxiomSchemas,
  val deductionRules: Map[String, DeductionRule] = DeductionRule.naturalDeductionRules)(
  implicit val M: MonadPlus[M]) {
  lazy val isValid = validation.forall(_ != M.empty)

  lazy val validation: List[M[SequentValidation]] = validateProof(proof.reverse).reverse

  private[this] def validateProof(seqs: List[Sequent]): List[M[SequentValidation]] = seqs match {
    case Nil => Nil
    case statement :: before => {
      val axiomaticValidations = {
        val goodAxioms = for {
          (name, axiom) <- axioms.toList
          if axiom.hasMatch(statement)
        } yield ValidatedAxiom(statement, axiom, name)
        M.unite(M.point(goodAxioms))
      }
      val deducedValidations = {
        val valids = for {
          (name, rule) <- deductionRules.toList
          DeductionRule(premises, conclusion) = rule
          if conclusion.hasMatch(statement)
          candidatePremises <- premises.map(ps => before.filter(ps.hasMatch)).sequence
          candidateDeduction = Deduction(candidatePremises, statement)
          if rule.hasMatch(candidateDeduction)
        } yield ValidatedDeduction(candidateDeduction, rule, name)
        M.unite(M.point(valids))
      }
      val validations = M.plus(axiomaticValidations, deducedValidations)
      validations :: validateProof(before)
    }
  }
}