package deduction 

sealed abstract class FormulaSchema extends Schema[Formula]
case object FormulaSchema extends ComplexParsable[FormulaSchema] {
  val startSymbol = "FS"
  val synchronousProductions: Map[List[Parsable[_]], (List[AST] => Option[FormulaSchema])] = Map(
    List(Word) -> 
      (c => for {
        w <- Word.fromAST(c(0))
      } yield ArbitraryFormulaSchema(w)),
    List(Terminal("¬"), FormulaSchema) -> 
      (c => for {
        f <- FormulaSchema.fromAST(c(1))
      } yield NegationSchema(f)),
    List(Terminal("("), FormulaSchema, ConnectiveSchema, FormulaSchema, Terminal(")")) -> 
      (c => for {
        f <- FormulaSchema.fromAST(c(1))
        op <- ConnectiveSchema.fromAST(c(2))
        g <- FormulaSchema.fromAST(c(3))
      } yield CompoundSchema(op, f, g))
  )
}
case class ArbitraryFormulaSchema(name: String) extends FormulaSchema {
  override def matches(targ: Formula) = Map(name -> targ) :: Nil
  override val toString = name
}

// as of yet unused
case class AtomSchema(name: String) extends FormulaSchema {
  override def matches(targ: Formula) = targ match {
    case Atom(a) => Map(name -> a) :: Nil
    case _       => Nil
  }
  override val toString = name
}
case class NegationSchema(f: FormulaSchema) extends FormulaSchema {
  override def matches(targ: Formula) = targ match {
    case Negation(negated) => f.matches(negated)
    case _                 => Nil
  }
  override val toString = s"¬$f"
}
case class CompoundSchema(c: ConnectiveSchema, f: FormulaSchema, g: FormulaSchema) extends FormulaSchema {
  override def matches(targ: Formula) = targ match {
    case Compound(op, first, second) => {
      val connectiveNamings = c.matches(op)
      val firstNamings = f.matches(first)
      val secondNamings = g.matches(second)
      consistentNamings(connectiveNamings :: firstNamings :: secondNamings :: Nil)
    }
    case _ => Nil
  }
  override val toString = s"($f $c $g)"
}

sealed abstract class ConnectiveSchema extends Schema[Connective]
case object ConnectiveSchema extends ComplexParsable[ConnectiveSchema] {
  val startSymbol = "CS"
  val synchronousProductions: Map[List[Parsable[_]], (List[AST] => Option[ConnectiveSchema])] = Map(
    List(Word) -> 
      (c => for {
        w <- Word.fromAST(c(0))
      } yield ArbitraryConnectiveSchema(w)),
    List(Connective) -> 
      (c => for {
        c <- Connective.fromAST(c(0))
      } yield SpecifiedConnectiveSchema(c))
  )
}
case class ArbitraryConnectiveSchema(name: String) extends ConnectiveSchema {
  override def matches(targ: Connective) = Map(name -> targ) :: Nil
  override val toString = name
}
case class SpecifiedConnectiveSchema(c: Connective) extends ConnectiveSchema {
  def matches(targ: Connective) = if (c.equals(targ)) Map[String, Any]() :: Nil else Nil
  override val toString = c.toString
}
