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
    case IDTOK(_)     =>  true
    case NUMTOK(_)    =>  true
    case otherTok     =>
      List(
        TRUETOK(),
        FALSETOK(),
        SUCCTOK(),
        PREDTOK(),
        ISZEROTOK(),
        IFTOK(),
        FUNTOK(),
        RECTOK(),
        LPARENTOK(),
        LETTOK())
        .contains(otherTok)
  }

  private def parseExps(toks: List[TOK]): (TERM, List[TOK]) = {

    def parserexp_aux(t: (TERM, List[TOK])): (TERM, List[TOK]) = t match {
      case (ERROR(reason), _)         =>  (ERROR(reason), toks)
      case (term1, tok1 :: toks1) if startsExp(tok1.asInstanceOf[TOK])  =>  parseExpression(tok1::toks1) match {
        case (ERROR(reason), toks2)       =>  (ERROR(reason), toks2)
        case (term2, toks2)               =>  parserexp_aux(APP(term1, term2), toks2)
      }
      case (term1, toks1)             => (term1, toks1)
    }

    parserexp_aux(parseExpression(toks))
  }

  private def parseIFExpression(xs: List[TOK]): (TERM, List[TOK]) = parseExps(xs) match {
    case (ERROR(s), r)                        =>  (ERROR(s), r)
    case (guard, THENTOK() :: toks1)          =>  parseExps(toks1) match {
      case(ERROR(s), toks2)                     =>  (ERROR(s), toks2)
      case (thenbranch, ELSETOK() :: toks2)     =>  parseExps(toks2) match {
        case (ERROR(s), toks3)                    =>  (ERROR(s), toks3)
        case (elsebranch, toks3)                  =>  (IF(guard, thenbranch, elsebranch), toks3)
      }
      case (thenbranch, tok2 :: toks2)          =>  (ERROR(s"Expected 'else', found $tok2"), List(EOF()))
      case (thenbranch, Nil)                    =>  (ERROR("Lexer error: missing EOF token"), List(EOF()))
    }
    case (guard, tok1:: toks1)                =>  (ERROR(s"Expected 'then', found $tok1"), toks1)
    case (guard, Nil)                         =>  (ERROR("Lexer error: missing EOF token"), List(EOF()))
  }

  private def parseFUNExprextion(xs: List[TOK]): (TERM, List[TOK]) = xs match {
    case IDTOK(x) :: ARROWTOK() :: toks           => parseExps(toks) match {
      case (ERROR(s), toks1)                        => (ERROR(s), toks1)
      case (body, toks1)                            => (FUNC(x, body), toks1)
    }
    case IDTOK(x) :: tok :: toks                  =>  (ERROR(s"Expected '->' after fun $x, found $tok"), List(EOF()))
    case IDTOK(x) :: Nil                          =>  (ERROR("Lexer error: missing EOF token"), List(EOF()))
    case tok :: _                                 =>  (ERROR(s"Expected identifier after 'fun', found $tok"), List(EOF()))
    case Nil                                      =>  (ERROR("Lexer error: missing EOF token"), List(EOF()))
  }

  def parseRECExpression(xs: List[TOK]): (TERM, List[TOK]) = xs match {
    case IDTOK(x) :: ARROWTOK() :: toks           =>  parseExps(toks) match {
      case (ERROR(s), toks1)                        =>  (ERROR(s), toks1)
      case (body, toks1)                            =>  (REC(x, body), toks1)
    }
    case IDTOK(x) :: tok :: toks                  =>  (ERROR(s"Expected '->' after rec $x, found $tok"), List(EOF()))
    case IDTOK(_) :: Nil                          =>  (ERROR(s"Lexer error: missing EOF token"), List(EOF()))
    case tok :: toks                              =>  (ERROR(s"Expected indentifier after 'rec'm found $tok"), List(EOF()))
    case Nil                                      =>  (ERROR(s"Lexer error: missing EOF token"), List(EOF()))
  }

  def parserLetExpression(xs: List[TOK]): (TERM, List[TOK]) = xs match {
    case IDTOK(x) :: EQUALTOK() :: toks           =>  parseExps(toks) match {
      case (ERROR(s), toks1)                        =>  (ERROR(s), toks1)
      case (term1, INTOK() :: toks1)                =>  parseExps(toks1) match {
        case (ERROR(s), toks2)                        =>  (ERROR(s), toks2)
        case (term2, toks2)                           =>  (APP(FUNC(x, term2), term1), toks2)
      }
      case (term1, tok1 :: toks1)                   =>  (ERROR(s"Expected 'in', found $tok1"), List(EOF()))
      case (term1, Nil)                             =>  (ERROR(s"Lexer error: missing EOF token"), List(EOF()))
    }
    case IDTOK(x) :: tok :: toks                  =>  (ERROR(s"Expected '=' after let $x, found $tok"), List(EOF()))
    case IDTOK(_) :: Nil                          =>  (ERROR(s"Lexer error: missing EOF token"), List(EOF()))
    case tok :: toks                              =>  (ERROR(s"Expected indentifier after 'let', found $tok"), List(EOF()))
    case _                                        =>  (ERROR(s"Lexer error: missing EOF token"), List(EOF()))
  }

  private def parseExpression(t: List[TOK]): (TERM, List[TOK]) = t match {
    case IDTOK(x)   ::toks                    =>  (ID(x), toks)
    case NUMTOK(n)  ::toks                    =>  (NUM(n), toks)
    case TRUETOK()  ::toks                    =>  (BOOL(true), toks)
    case FALSETOK() ::toks                    =>  (BOOL(false), toks)
    case SUCCTOK()  ::toks                    =>  (SUCC(), toks)
    case PREDTOK()  ::toks                    =>  (PRED(), toks)
    case ISZEROTOK()::toks                    =>  (ISZERO(), toks)

    case IFTOK()    ::toks                    =>  parseIFExpression(toks)
    case FUNTOK()   ::toks                    =>  parseFUNExprextion(toks)
    case RECTOK()   ::toks                    =>  parseRECExpression(toks)
    case LETTOK()   ::toks                    =>  parserLetExpression(toks)

    case LPARENTOK()::left                    => parseExps(left) match {
      case (ERROR(reason), l)                  =>  (ERROR("After '(' an valid expression has to follow"), l)
      case (exp, RPARENT()::l)                 =>  (exp, l)
      case (exp, tok::l)                       =>  (ERROR("After ')' expresion"), List(EOF()))
    }

    case tok::toks                            =>  (ERROR("Expected Expression but found: " + tok), List(EOF()))
  }

  private def parse(toks: List[TOK]) = parseExps(toks) match {
    case (ERROR(s), _)              =>  ERROR(s)
    case (term, EOF()::leftToks)    =>  term
    case (term, tok::toks1)         =>  ERROR("EOF expected, found: " + tok)
    case (term, Nil)                =>  ERROR("Lexer error: missing EOF token")
  }

  def parseStr(sourceCode: String) = sourceCode |> Lexer.lexerStr |> parse

  def parseFile(fileName: String) = scala.io.Source.fromFile(fileName ).mkString |> parseStr
}

