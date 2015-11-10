/**
  * Created by nperez on 11/10/15.
 */

package PCF.Parser

import PCF.Parser.AST._
import PCF.Tokenizer._
import PCF.Pipes.IPipe._


object Parser {

  private def parseExpression(t: List[TOK]): (TERM, List[TOK]) = t match {
    case IDTOK(x)   ::toks    =>  (ID(x), toks)
    case NUMTOK(n)  ::toks    =>  (NUM(n), toks)
    case TRUETOK()  ::toks    =>  (BOOL(true), toks)
    case FALSETOK() ::toks    =>  (BOOL(false), toks)
    case SUCCTOK()  ::toks    =>  (SUCC(), toks)
    case PREDTOK()  ::toks    =>  (PRED(), toks)
    case ISZEROTOK()::toks    =>  (ISZERO(), toks)

    case IFTOK()    ::toks    =>  parseExpression(toks) match {
      case (ERROR(reason), left)      =>  (ERROR(reason), left)
      case (guard, THENTOK()::left)   =>  parseExpression(left) match {
        case (ERROR(reason), dontCare)          =>  (ERROR("IF requires an TERM after THEN"), dontCare)
        case (thenExp, ELSETOK()::left)         =>  parseExpression(left) match {
          case (ERROR(reason), left)                =>  (ERROR("IF requires an TERM after ELSE"), left)
          case (elseExp, left)                      => (IF(guard, thenExp, elseExp), left)
        }
        case (thenExpe, _)            =>  (ERROR("IF requires an ELSE"), left)
      }
      case (guard, tok)               =>  (ERROR("IF requires an TERM as guard"), List(EOF()))
    }

    case tok::toks            =>  (ERROR("Expected Expression but found: " + tok), List(EOF()))
  }

  private def parse(toks: List[TOK]) = parseExpression(toks) match {
    case (ERROR(s), _)              =>  ERROR(s)
    case (term, EOF()::leftToks)    =>  term
    case (term, tok::toks1)         =>  ERROR("EOF expected, found: " + tok)
    case (term, Nil)                =>  ERROR("Lexer error: missing EOF token")
  }

  def parseStr(sourceCode: String) = sourceCode |> Lexer.lexerStr |> parse
}

