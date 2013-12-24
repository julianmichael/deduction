package propositional.schema

import parsing._
import propositional._
import deduction.Schema

sealed abstract class AssumptionSchema extends Schema[Assumptions]
case object AssumptionSchema extends ComplexParsable[AssumptionSchema] {
  val startSymbol = "AS"
  val synchronousProductions: Map[List[Parsable[_]], (List[AST] => Option[AssumptionSchema])] = Map(
    List(Word) ->
      (c => for {
        w <- Word.fromAST(c(0))
      } yield ArbitrarySetSchema(w)),
    List(Terminal("|"), FormulaSchema) ->
      (c => for {
        f <- FormulaSchema.fromAST(c(1))
      } yield SingleFormulaSchema(f)),
    List(AssumptionSchema, Terminal(","), FormulaSchema) ->
      (c => for {
        a <- AssumptionSchema.fromAST(c(0))
        f <- FormulaSchema.fromAST(c(2))
      } yield AssumptionsPlusFormulaSchema(a, f)),
    List(AssumptionSchema, Terminal("∪"), AssumptionSchema) ->
      (c => for {
        a1 <- AssumptionSchema.fromAST(c(0))
        a2 <- AssumptionSchema.fromAST(c(2))
      } yield UnionSchema(a1, a2)))
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
    targ.set.subsets.toList flatMap {
      case first => {
        val requiredSubsetOfSecond = targ.set -- first
        first.subsets.toList flatMap {
          case subsetOfFirst => {
            val second = requiredSubsetOfSecond ++ subsetOfFirst
            val firstNamings = one.matches(Assumptions(first))
            val secondNamings = two.matches(Assumptions(second))
            consistentNamings(firstNamings :: secondNamings :: Nil)
          }
        }
      }
    }
  }
  override val toString = s"$one ∪ $two"
}
case class SingleFormulaSchema(formulaSchema: FormulaSchema) extends AssumptionSchema {
  override def matches(targ: Assumptions) = {
    if (targ.set.size != 1) Nil
    else {
      val formula = targ.set.toList(0)
      formulaSchema.matches(formula)
    }
  }
  override val toString = s"|$formulaSchema"
}
case object EmptySchema extends AssumptionSchema {
  override def matches(targ: Assumptions) = {
    if (targ.set.isEmpty) Map[String, Any]() :: Nil
    else Nil
  }
  override val toString = ""
}