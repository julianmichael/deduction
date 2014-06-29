package propositional.schema

import parsing._
import propositional._
import deduction.Schema

sealed abstract class ConnectiveSchema extends Schema[Connective]
case object ConnectiveSchema extends ComplexParsable[ConnectiveSchema] {
  val synchronousProductions: Map[List[Parsable[_]], (List[AST[Parsable[_]]] => Option[ConnectiveSchema])] = Map(
    List(Name) ->
      (c => for {
        w <- Name.fromAST(c(0))
      } yield ArbitraryConnectiveSchema(w)),
    List(Connective) ->
      (c => for {
        c <- Connective.fromAST(c(0))
      } yield SpecifiedConnectiveSchema(c)))
}
case class ArbitraryConnectiveSchema(name: String) extends ConnectiveSchema {
  override def matches(targ: Connective) = Map(name -> targ) :: Nil
  override val toString = name
}
case class SpecifiedConnectiveSchema(c: Connective) extends ConnectiveSchema {
  def matches(targ: Connective) = if (c.equals(targ)) Map[String, Any]() :: Nil else Nil
  override val toString = c.toString
}
