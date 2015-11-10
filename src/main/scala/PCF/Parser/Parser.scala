/**
  * Created by nperez on 11/10/15.
 */

package PCF.Parser

// This is the grammar of PCF

// Exp ::=  x | n
//            | true
//            | false
//            | succ
//            | pred
//            | iszero
//            | if Exps then Exps else Exps
//            | fun x -> Exps
//            | rec x -> Exps
//            | (Exps)
//            | let x = Exps in Exps
//            |
// Exps ::= Exps Exp | Exp
//
// We resolve the ambiguity in the grammar by making concatenation
// (i.e. function application) bind tighter than if, fun, rec, and let.

import PCF.Pipes.IPipe._
import PCF.Tokenizer._

abstract class TERM()

case class ID(val id: String) extends TERM
case class NUM(val value: Int) extends TERM
case class BOOL(val value: Boolean) extends TERM
case class SUCC() extends TERM
case class PRED() extends TERM
case class ISZERO() extends TERM
case class IF(condition: TERM, Then: TERM, Else: TERM) extends TERM
case class APP(function: TERM, value: TERM) extends TERM
case class FUNC(name: String, body: TERM) extends TERM
case class REC(name: String, body: TERM) extends TERM
case class ERROR(reason: String) extends TERM




object Parser {

  private def parseExpression(toks: List[TOK]): (TERM, List[TOK]) = ???

  private def parse(toks: List[TOK]) = parseExpression(toks) match {
    case (ERROR(s), _)              =>  ERROR(s)
    case (term, EOF()::leftToks)    =>  term
    case (term, tok::toks1)         =>  ERROR("EOF expected, found: " + tok)
    case (term, Nil)                =>  ERROR("Lexer error: missing EOF token")
  }

  def parseStr(sourceCode: String) = sourceCode |> Lexer.lexerStr |> parse
}
