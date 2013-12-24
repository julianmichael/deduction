package deduction

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import parsing._
import propositional._
import propositional.schema._
import propositional.SequentTestParameters
import propositional.FormulaTestParameters
import propositional.AssumptionsTestParameters
import propositional.ConnectiveTestParameters
import propositional.schema.FormulaSchemaTestParameters
import propositional.schema.ConnectiveSchemaTestParameters
import propositional.schema.SequentSchemaTestParameters
import propositional.schema.AssumptionSchemaTestParameters

@RunWith(classOf[JUnitRunner])
class TestSuite extends FunSuite {

  def testParsableWithParameters[A](params: ParsableTestParameters[A])(parsable: Parsable[A]) = {
    import params._
    test(s"$parsable children") {
      assert(parsable.children === children)
    }
    test(s"$parsable grammar productions") {
      assert(parsable.grammar.productions === productions)
    }
    test(s"$parsable CNF productions") {
      assert(parsable.grammar.cnfProductions === cnfProductions)
    }
    test(s"$parsable nonterminals") {
      assert(parsable.grammar.nonterminals === nonterminals)
    }
    test(s"$parsable terminals") {
      assert(parsable.grammar.terminals === terminals)
    }

    (goodStrings, goodTokenizations).zipped.foreach {
      case (string, toks) => test(s"$string tokenizing") {
        assert(parsable.grammar.tokenize(string) === toks)
      }
    }

    (goodTokenizations, goodASTs).zipped.foreach {
      case (toks, ast) => {
        test(s"AST for $parsable tokens $toks") {
          assert(parsable.grammar.parseTokens(toks).head === ast)
        }
      }
    }

    (goodStrings, goodASTs).zipped.foreach {
      case (string, ast) => {
        test(s"AST for $parsable $string") {
          assert(parsable.grammar.parse(string) === Some(ast))
        }
      }
    }

    def testASTSanity(ast: AST): Unit = {
      test(s"AST $ast has properly formed productions") {
        ast.children match {
          case Nil => assert(ast.production === None)
          case xs => {
            ast.production match {
              case None    => assert(false)
              case Some(p) => assert(productions(p) | parsable.openSymbols(p.label))
            }
          }
        }
      }
//      ast.children.foreach(testASTSanity)
    }

    goodASTs.foreach(testASTSanity)

    (goodASTs, goodSymbolics).zipped.foreach {
      case (ast, symbolic) => {
        test(s"Symbolic representation of AST for $parsable $symbolic") {
          assert(parsable.fromAST(ast) === Some(symbolic))
        }
      }
    }

    (goodStrings, goodSymbolics).zipped.foreach {
      case (string, symbolic) => {
        test(s"Symbolic representation of $parsable $string") {
          assert(parsable.fromString(string) === Some(symbolic))
        }
      }
    }
  }

  testParsableWithParameters(ConnectiveTestParameters)(Connective)
  testParsableWithParameters(FormulaTestParameters)(Formula)
  testParsableWithParameters(AssumptionsTestParameters)(Assumptions)
  testParsableWithParameters(SequentTestParameters)(Sequent)

  testParsableWithParameters(ConnectiveSchemaTestParameters)(ConnectiveSchema)
  testParsableWithParameters(FormulaSchemaTestParameters)(FormulaSchema)
  testParsableWithParameters(AssumptionSchemaTestParameters)(AssumptionSchema)
  testParsableWithParameters(SequentSchemaTestParameters)(SequentSchema)
}