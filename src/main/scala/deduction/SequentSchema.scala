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
case object SequentSchema extends ComplexParsable[SequentSchema] {
  val startSymbol = "SS"
  val synchronousProductions: Map[List[Parsable[_]], (List[AST] => Option[SequentSchema])] = Map(
    List(AssumptionSchema, Terminal("⇒"), FormulaSchema) -> 
      (c => for {
        a <- AssumptionSchema.fromAST(c(0))
        f <- FormulaSchema.fromAST(c(2))
      } yield SequentSchema(a, Some(f))),
    List(Terminal("⇒"), FormulaSchema) -> 
      (c => for {
        f <- FormulaSchema.fromAST(c(1))
      } yield SequentSchema(EmptySchema, Some(f))),
    List(AssumptionSchema, Terminal("⇒")) -> 
      (c => for {
        a <- AssumptionSchema.fromAST(c(0))
      } yield SequentSchema(a, None))
  )
}

sealed abstract class AssumptionSchema extends Schema[Assumptions]
case object AssumptionSchema extends ComplexParsable[AssumptionSchema] {
  val startSymbol = "AS"
  val synchronousProductions: Map[List[Parsable[_]], (List[AST] => Option[AssumptionSchema])] = Map(
    List(Word) -> 
      (c => for {
        w <- Word.fromAST(c(0))
      } yield ArbitrarySetSchema(w)),
    List(FormulaSchema) -> 
      (c => for {
        f <- FormulaSchema.fromAST(c(0))
      } yield SingleFormulaSchema(f)),
    List(AssumptionSchema, Terminal(","), FormulaSchema) -> 
      (c => for {
        a <- AssumptionSchema.fromAST(c(0))
        f <- FormulaSchema.fromAST(c(2))
      } yield AssumptionsPlusFormulaSchema(a, f)),
    List(AssumptionSchema, Terminal("∪"), AssumptionSchema) -> 
      (c => for{
        a1 <- AssumptionSchema.fromAST(c(0))
        a2 <- AssumptionSchema.fromAST(c(2))
      } yield UnionSchema(a1, a2))
  )
}
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
