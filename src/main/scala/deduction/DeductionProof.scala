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

  def prettyString: String = {
    // let's go mutable instead of using the state monad and see how it goes
    import scala.collection.mutable
    import propositional._
    var out = new StringBuilder
    var nextLine = 1

    val sequentToLine = mutable.Map.empty[Sequent, Int]
    val formulaToAssumptionLabel = mutable.Map.empty[Formula, String]
    def assumptionLabel(f: Formula) = {
      if(f.size <= 1) f.toString
      else if(formulaToAssumptionLabel.contains(f)) formulaToAssumptionLabel(f)
      else {
        val newLabel = s"A${formulaToAssumptionLabel.size}"
        out ++= s"${newLabel}. $f\n"
        formulaToAssumptionLabel.put(f, newLabel)
        newLabel
      }
    }

    (proof, validation).zipped.foreach { case(s@Sequent(Assumptions(asmp), conclusion), valid) => {
      sequentToLine.put(s, nextLine)
      val assumptionLabels = asmp.map(assumptionLabel).mkString(", ")
      val conclusionLabel = conclusion.getOrElse("")
      val sequentLabel = s"$assumptionLabels => $conclusionLabel"
      out ++= f"$nextLine%3d. $sequentLabel%-50s"
      val ruleLabelM = valid.map {
        case ValidatedAxiom(_, _, name) => out ++= s"($name)"
        case ValidatedDeduction(Deduction(premises, _), _, name) => {
          val lineNumbers = premises map (sequentToLine) mkString(", ")
          out ++= s"($name, $lineNumbers)"
        }
      }
      if(nextLine % 5 == 0) out ++= "\n\n"
      else out ++= "\n"
      nextLine = nextLine + 1
    }}
    // it went pretty well
    out.toString
  }
}