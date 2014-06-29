package propositional
import parsing.ParsableLexicalCategory

object AtomicFormula extends ParsableLexicalCategory({ a =>
  Formula.allTokens.forall(tok => !a.contains(tok))
})
