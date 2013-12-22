package deduction

trait Parsable[A] {
  def startSymbol: String
  def openSymbols: Set[String] = Set()
  def synchronousProductions: Map[Production, (List[AST] => Option[A])]
  def children: Set[Parsable[_]]
  def productions: Set[Production] =
    children.foldLeft(synchronousProductions.keySet)(_ ++ _.productions)
  def grammar: Grammar = new Grammar(productions, Some(startSymbol), openSymbols ++ children.flatMap(_.openSymbols))
  def fromString(s: String) = grammar.parse(s) flatMap fromAST
  def fromAST(ast: AST): Option[A] = {
    for {
      p <- ast.production
      func <- synchronousProductions.get(p)
      item <- func(ast.children)
    } yield item
  }
}