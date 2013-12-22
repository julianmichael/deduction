package deduction

case class SequentSchema(
  val assumptionSchema: AssumptionSchema,
  val conclusion: Option[FormulaSchema]) extends Schema[Sequent] {
  def matches(targ: Sequent): List[Map[String, Any]] = targ match {
    case Sequent(assumptions, formula) => {
      val assumptionNamings = assumptionSchema.matches(assumptions)
      val formulaNamings = (conclusion, formula) match {
        case (Some(conc), Some(form)) => conc.matches(form)
        case (None, None)             => Map[String, Any]() :: Nil
        case _                        => Nil
      }
      consistentNamings(assumptionNamings :: formulaNamings :: Nil)
    }
  }
  
  override def toString = {
    val assumptionsString = assumptionSchema.toString
    val conclusionString = conclusion match {
      case None => ""
      case Some(f) => f.toString
    }
    s"$assumptionsString ⇒ $conclusionString"
  }
}

sealed abstract class AssumptionSchema extends Schema[Assumptions]
case class ArbitrarySetSchema(name: String) extends AssumptionSchema {
  override def matches(targ: Assumptions) = Map(name -> targ) :: Nil
  override val toString = name
}
case class AssumptionsPlusFormulaSchema(remainingAssumptionSchema: AssumptionSchema, formulaSchema: FormulaSchema) extends AssumptionSchema {
  override def matches(targ: Assumptions) = {
    targ.set.toList flatMap { formula =>
      {
        val remainingAssumptions = targ.set - formula
        val remainingAssumptionNamings = remainingAssumptionSchema.matches(Assumptions(remainingAssumptions))
        val formulaNamings = formulaSchema.matches(formula)
        consistentNamings(remainingAssumptionNamings :: formulaNamings :: Nil)
      }
    }
  }
  override val toString = s"$remainingAssumptionSchema, $formulaSchema"
}
case class UnionSchema(one: AssumptionSchema, two: AssumptionSchema) extends AssumptionSchema {
  override def matches(targ: Assumptions) = {
    targ.set.subsets.toList flatMap { first =>
      {
        val second = targ.set -- first
        val firstNamings = one.matches(Assumptions(first))
        val secondNamings = two.matches(Assumptions(second))
        consistentNamings(firstNamings :: secondNamings :: Nil)
      }
    }
  }
  override val toString = s"$one ∪ $two"
}
case class SingleFormulaSchema(formulaSchema: FormulaSchema) extends AssumptionSchema {
  override def matches(targ: Assumptions) = {
    if(targ.set.size != 1) Nil
    else {
      val formula = targ.set.toList(0)
      formulaSchema.matches(formula)
    }
  }
}
case object EmptySchema extends AssumptionSchema {
  override def matches(targ: Assumptions) = {
    if (targ.set.isEmpty) Map[String, Any]() :: Nil
    else Nil
  }
  override val toString = ""
}
