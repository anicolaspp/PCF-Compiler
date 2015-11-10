/**
  * Created by nperez on 11/10/15.
 */

package PCF.Parser

import PCF.Parser.AST._
import PCF.Tokenizer._
import PCF.Pipes.IPipe._


object Parser {

  private def parseExpression(t: List[TOK]): (TERM, List[TOK]) = t match {
    case IDTOK(x)::toks     =>  (ID(x), toks)
    case NUMTOK(n)::toks    =>  (NUM(n), toks)
    case TRUETOK()::toks    =>  (BOOL(true), toks)
    case FALSETOK()::toks   =>  (BOOL(false), toks)
    case SUCCTOK()::toks    =>  (SUCC(), toks)
    case PREDTOK()::toks    =>  (PRED(), toks)
    case ISZEROTOK()::toks  =>  (ISZERO(), toks)
  }

  private def parse(toks: List[TOK]) = parseExpression(toks) match {
    case (ERROR(s), _)              =>  ERROR(s)
    case (term, EOF()::leftToks)    =>  term
    case (term, tok::toks1)         =>  ERROR("EOF expected, found: " + tok)
    case (term, Nil)                =>  ERROR("Lexer error: missing EOF token")
  }

  def parseStr(sourceCode: String) = sourceCode |> Lexer.lexerStr |> parse
}
