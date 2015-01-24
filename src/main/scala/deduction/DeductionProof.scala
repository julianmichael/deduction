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
  implicit val M: MonadPlus[M]
  ){
  lazy val isValid = validation.forall(_ != M.empty)
  
  lazy val validation: List[M[SequentValidation]] = validationOfSequentList(proof.reverse).reverse

  private[this] def msum[A](iter: Iterable[M[A]]): M[A] = {
    iter.foldLeft[M[A]](M.empty)(M.plus(_, _))
  }
  private[this] def msum[A](iter: Iterator[M[A]]): M[A] = {
    iter.foldLeft[M[A]](M.empty)(M.plus(_, _))
  }

  private def validationOfSequentList(sequents: List[Sequent]): List[M[SequentValidation]] = sequents match {
    case Nil => Nil
    case statement :: before => {
      val restOfProof = validationOfSequentList(before)
      val axiomaticValidations = {
        val valids = axioms.map {
          case (name, axiom) if axiom.hasMatch(statement) =>
            M.point(ValidatedAxiom(statement, axiom, name))
          case _ =>
            M.empty
        }
        msum(valids)
      }
      // now the rough part. we want to see if, for each deduction rule...
      val deducedValidations = {
        val valids = deductionRules.map {
          case (name, rule) => rule match {
            case DeductionRule(premises, conclusion) => {
              // If the conclusion has some matching...
              if(!conclusion.hasMatch(statement)) {
                M.empty
              } else {
                // for all the combinations of premises that match the general form of a premise...
                val candidateSequentsPerPremise = premises.map {
                  case premiseSchema => before.filter(premiseSchema.hasMatch)
                }
                val validations = candidateSequentsPerPremise.sequence.iterator.map {
                  case candidatePremises => {
                    // boxing the choice of premises and the current statement together...
                    val candidateDeduction = Deduction(candidatePremises, statement)
                    // if the deduction rule matches this deduction, then we add the validation.
                    if (rule.hasMatch(candidateDeduction))
                      M.point(ValidatedDeduction(candidateDeduction, rule, name))
                    else
                      M.empty
                  }
                }
                val res = msum(validations)
                res
              }
            }
          }
        }
        msum(valids)
      }
      val validations = M.plus(axiomaticValidations, deducedValidations)
      validations :: validationOfSequentList(before)
    }
  }
}