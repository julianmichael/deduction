package propositional.schema

import parsing._
import propositional._
import deduction.Schema

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
