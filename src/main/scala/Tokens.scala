/**
 * Created by nperez on 11/9/15.
 */

package PCF.Tokenizer

import PCF.Pipes.IPipe._

abstract class TOK()

case class IDTOK(val id: String) extends TOK
case class NUMTOK(val value: Int)extends TOK
case class TRUETOK()extends TOK
case class FALSETOK()extends TOK
case class SUCCTOK()extends TOK
case class PREDTOK()extends TOK
case class ISZERO()extends TOK
case class IFTOK()extends TOK
case class THENTOK()extends TOK
case class ELSETOK()extends TOK
case class FUNTOK()extends TOK
case class RECTOK()extends TOK
case class ARROWTOK()extends TOK
case class LPARENTOK()extends TOK
case class RPARENT()extends TOK
case class LETTOK()extends TOK
case class EQUALTOK()extends TOK
case class INTOK()extends TOK
case class EOF() extends TOK

object  Lexer {
  def explode (s: String) = s.toCharArray.toList

  def isWhite(c: Char) =  c == ' ' || c == '\n' || c == '\r' || c == '\t'

  def isAlpha(c: Char) = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

  def isDigit(c: Char) = '0' <= c && c <= '9'

  def keyword(s: String): TOK = s match {
    case "true"   =>  TRUETOK()
    case "false"  =>  FALSETOK()
    case "succ"   =>  SUCCTOK()
    case "pred"   =>  PREDTOK()
    case "iszero" =>  ISZERO()
    case "if"     =>  IFTOK()
    case "then"   =>  THENTOK()
    case "else"   =>  ELSETOK()
    case "fun"    =>  FUNTOK()
    case "rec"    =>  RECTOK()
    case "let"    =>  LETTOK()
    case "in"     =>  INTOK()
    case ident    =>  IDTOK(ident)
  }

  def getId(ident: String, l: List[Char]): (TOK, List[Char])  = l match {
    case c :: cs if (isAlpha(c) || isDigit(c))  =>  getId(ident + c, cs)
    case cs                                     =>  (keyword(ident), cs)
  }

  def getNum(num: Int, l: List[Char]): (TOK, List[Char]) = l match {
    case c :: cs if isDigit(c)    =>  getNum(10*num + c.toInt - 0, cs)
    case cs                       =>  (NUMTOK(num), cs)
  }

  def getTok(l: List[Char]): (TOK, List[Char]) = l match {
    case Nil => (EOF(), List())
    case c :: cs if isWhite(c) => getTok(cs)
    case c :: cs if isAlpha(c) => getId(c.toString, cs)
    case c :: cs if isDigit(c) => getNum(c.toInt - 0, cs)
    case '(' :: cs => (LPARENTOK(), cs)
    case ')' :: cs => (RPARENT(), cs)
    case '=' :: cs => (EQUALTOK(), cs)
    case '-' :: cs => (EQUALTOK(), cs)
    case '#' :: cs =>
      def skipLine(x: List[Char]): (TOK, List[Char]) = x match {
        case null => (EOF(), List())
        case '\n' :: cs1 => getTok(cs1)
        case _ :: cs1 => skipLine(cs1)
      }
      skipLine(cs)
    case c :: cs => {
      println("Skipping illegal character: " + c)
      getTok(cs)
    }
  }

  def tokenize(cs: List[Char]): List[TOK] = {
    def getToks(toks: List[TOK], cs: List[Char]): List[TOK] = getTok(cs) match {
      case (EOF(), cs) => (EOF() :: toks).reverse
      case (tok, cs) => getToks(tok :: toks, cs)
    }
    getToks(List(), cs)
  }

  def lexerStr(sourceCode: String): List[TOK] = sourceCode |> explode |> tokenize

  def lexerFile(fileName: String): List[TOK] = scala.io.Source.fromFile(fileName).mkString |> explode |> tokenize
}



