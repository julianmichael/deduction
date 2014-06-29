package propositional.schema
import parsing.ParsableLexicalCategory

object Name extends ParsableLexicalCategory({ a =>
  SequentSchema.allTokens.forall(tok => !a.contains(tok))
})
