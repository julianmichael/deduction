package deduction 

case class Deduction(premises: List[Sequent], conclusion: Sequent)

case class DeductionRule(
    premiseSchemas: List[SequentSchema],
    conclusionSchema: SequentSchema) extends Schema[Deduction] {
  def matches(targ: Deduction) = targ match {
    case Deduction(premises, conclusion) => {
      if(premises.length != premiseSchemas.length) {
        Nil
      }
      else {
        val premiseNamingsList = (premiseSchemas, premises).zipped.map(_.matches(_))
        val conclusionNamings = conclusionSchema.matches(conclusion)
        consistentNamings(conclusionNamings :: premiseNamingsList)
      }
    }
  }
  
  override val toString = {
    val premiseString = premiseSchemas match {
      case Nil => ""
      case list => list.mkString(", ")
    }
    val conclusionString = conclusionSchema.toString
    s"$premiseString | $conclusionString"
  }
}
object DeductionRule {
  val naturalDeductionRules = Map[String, DeductionRule](
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
