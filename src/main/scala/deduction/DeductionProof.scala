package deduction

import propositional.Sequent
import propositional.schema.SequentSchema

class DeductionProof(
  val goal: Sequent,
  val proof: List[Sequent],
  val axioms: Map[String, SequentSchema] = DeductionRule.naturalDeductionAxiomSchemas,
  val deductionRules: Map[String, DeductionRule] = DeductionRule.naturalDeductionRules) {
  val isValid = validation.forall(!_.isEmpty)
  
  lazy val validation: List[List[SequentValidation]] = validationOfSequentList(proof.reverse).reverse

  private def validationOfSequentList(sequents: List[Sequent]): List[List[SequentValidation]] = sequents match {
    case Nil => Nil
    case statement :: before => {
      val restOfProof = validationOfSequentList(before)
      val axiomaticValidations = axioms.flatMap {
        case (name, axiom) if !axiom.matches(statement).isEmpty => Some(ValidatedAxiom(statement, axiom, name))
        case _ => None
      }
      // now the rough part. we want to see if, for each deduction rule...
      val deducedValidations = deductionRules.flatMap {
        case (name, rule) => rule match {
          case DeductionRule(premises, conclusion) => {
            // for each (ordered) choice of #premises from `before` in the proof...
            val candidatePremiseList = before.combinations(premises.length).flatMap(_.permutations).toList
            val validations = candidatePremiseList.flatMap {
              case candidatePremises => {
                // boxing the choice of premises and the current statement together...
                val candidateDeduction = Deduction(candidatePremises, statement)
                // if the deduction rule matches this deduction, then we add the validation.
                if (!rule.matches(candidateDeduction).isEmpty)
                  Some(ValidatedDeduction(candidateDeduction, rule, name))
                else
                  None
              }
            }
            if (validations.isEmpty)
              Nil
            else
              validations
          }
        }
      }
      val validations = axiomaticValidations.toList ++ deducedValidations.toList
      validations :: validationOfSequentList(before)
    }
  }
}