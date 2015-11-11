/**
  * Created by nperez on 11/10/15.
 */

package PCF.Parser

import PCF.Parser.AST._
import PCF.Tokenizer._
import PCF.Pipes.IPipe._

// This is the grammar of PCF

// Exp ::=  x | n                               case (1)
//            | true                            case (2)
//            | false                           case (3)
//            | succ                            case (4)
//            | pred                            case (5)
//            | iszero                          case (6)
//            | if Exps then Exps else Exps     case (7)
//            | fun x -> Exps                   case (8)
//            | rec x -> Exps                   case (9)
//            | (Exps)                          case (10)
//            | let x = Exps in Exps            case (11)    translates to (fun x -> b)(a)
//            |
// Exps ::= Exps Exp | Exp
//
// We resolve the ambiguity in the grammar by making concatenation
// (i.e. function application) bind tighter than if, fun, rec, and let.

object Parser {

  private def startsExp(t: TOK) = t match{
    case IDTOK(x) =>  true
    case NUMTOK(n)=>  true
    case tok      =>  List(TRUETOK(), FALSETOK(), SUCCTOK(), PREDTOK(), ISZEROTOK(), IFTOK(), FUNTOK(), RECTOK(), LPARENTOK(), LETTOK()).contains(tok)
  }

  private def parseExps(toks: List[TOK]): (TERM, List[TOK]) = {
    def parserExps_aux(t: (TERM, List[TOK])): (TERM, List[TOK]) = t match {
      case (ERROR(reason), toks1)               =>  (ERROR(reason), toks)
      case (exp, tok1::toks1) if startsExp(tok1.asInstanceOf[TOK])  =>  parseExps((tok1::toks1)) match {
        case (ERROR(reason), toks2)   =>  (ERROR(reason), toks2)
        case (exp2, toks2)            =>  parserExps_aux(APP(exp.asInstanceOf[TERM], exp2), toks2)
      }
      case (exp, toks1)       => (exp, toks1)
    }
    parserExps_aux(parseExpression(toks))
  }

  private def parseExpression(t: List[TOK]): (TERM, List[TOK]) = t match {
    case IDTOK(x)   ::toks                    =>  (ID(x), toks)
    case NUMTOK(n)  ::toks                    =>  (NUM(n), toks)
    case TRUETOK()  ::toks                    =>  (BOOL(true), toks)
    case FALSETOK() ::toks                    =>  (BOOL(false), toks)
    case SUCCTOK()  ::toks                    =>  (SUCC(), toks)
    case PREDTOK()  ::toks                    =>  (PRED(), toks)
    case ISZEROTOK()::toks                    =>  (ISZERO(), toks)

    case IFTOK()    ::toks                    =>  parseExps(toks) match {
      case (ERROR(reason), left)                  =>  (ERROR(reason), left)
      case (guard, THENTOK()::left)               =>  parseExps(left) match {
        case (ERROR(reason), dontCare)                =>  (ERROR("IF requires an TERM after THEN"), dontCare)
        case (thenExp, ELSETOK()::left)               =>  parseExps(left) match {
          case (ERROR(reason), left)                      =>  (ERROR("IF requires an TERM after ELSE"), left)
          case (elseExp, left)                            => (IF(guard, thenExp, elseExp), left)
        }
        case (thenExpe, _)                            =>  (ERROR("IF requires an ELSE"), left)
      }
      case (guard, tok)                           =>  (ERROR("IF requires an TERM as guard"), List(EOF()))
    }

    case FUNTOK()::IDTOK(x)::ARROWTOK():: left=>  parseExps(left) match {
      case (ERROR(reason), left)                  =>  (ERROR("Expecting FUN body"), left)
      case (body, left)                           =>  (FUNC(x, body), left)
    }
    case FUNTOK()::IDTOK(x)::tok::left        =>  (ERROR("Expected '->' after FUN"), List(EOF()))
    case FUNTOK()::tok::left                  =>  (ERROR("Expecting identifier after FUN"), List(EOF()))

    case RECTOK()::IDTOK(x)::ARROWTOK()::left => parseExps(left) match {
      case (ERROR(reason), left)                  =>  (ERROR("Expecting REC body"), left)
      case (body, left)                           =>  (REC(x, body), left)
    }
    case RECTOK()::IDTOK(x)::tok::left        =>  (ERROR("Expected '->' after REC"), List(EOF()))
    case RECTOK()::tok::left                  =>  (ERROR("Expecting identifier after REC"), List(EOF()))

    case LETTOK()::IDTOK(x)::EQUALTOK()::left =>  parseExps(left) match {
      case (ERROR(reason), left)                  =>  (ERROR("Expecting LET body"), left)
      case (exp, INTOK()::left)                   =>  parseExps(left) match {
        case (ERROR(reason), left)                    =>  (ERROR("LET requires valid expression after IN"), left)
        case (freeExp, left)                          =>  ((APP(FUNC(x, freeExp), exp)), left)
      }
      case (exp, tok::left)                       =>  (ERROR("LET requires IN"), left)
    }
    case LETTOK()::IDTOK(x)::left             => (ERROR("LET requires '=' after variable"), left)
    case LETTOK()::left                       => (ERROR("LET requires variable definition"), left)

    case LPARENTOK()::left                    => parseExps(left) match {
      case (ERROR(reason), left)                  =>  (ERROR("After '(' an valid expression has to follow"), left)
      case (exp, RPARENT()::left)                 =>  (exp, left)
      case (exp, tok::left)                       =>  (ERROR("After ')' expresion"), List(EOF()))
    }

    case tok::toks                            =>  (ERROR("Expected Expression but found: " + tok), List(EOF()))
  }

  private def parse(toks: List[TOK]) = parseExpression(toks) match {
    case (ERROR(s), _)              =>  ERROR(s)
    case (term, EOF()::leftToks)    =>  term
    case (term, tok::toks1)         =>  ERROR("EOF expected, found: " + tok)
    case (term, Nil)                =>  ERROR("Lexer error: missing EOF token")
  }

  def parseStr(sourceCode: String) = sourceCode |> Lexer.lexerStr |> parse
}

