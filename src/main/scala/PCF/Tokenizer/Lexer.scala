/**
 * Created by nperez on 11/9/15.
 */

package PCF.Tokenizer

import PCF.Pipes.IPipe._

object Lexer {
  private def explode (s: String) = s.toCharArray.toList

  private def isWhite(c: Char) =  c == ' ' || c == '\n' || c == '\r' || c == '\t'

  private def isAlpha(c: Char) = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

  private def isDigit(c: Char) = '0' <= c && c <= '9'

  private def keyword(s: String): TOK = s match {
    case "true"   =>  TRUETOK()
    case "false"  =>  FALSETOK()
    case "succ"   =>  SUCCTOK()
    case "pred"   =>  PREDTOK()
    case "iszero" =>  ISZEROTOK()
    case "if"     =>  IFTOK()
    case "then"   =>  THENTOK()
    case "else"   =>  ELSETOK()
    case "fun"    =>  FUNTOK()
    case "rec"    =>  RECTOK()
    case "let"    =>  LETTOK()
    case "in"     =>  INTOK()
    case ident    =>  IDTOK(ident)
  }

  private def getId(ident: String, l: List[Char]): (TOK, List[Char])  = l match {
    case c :: cs if (isAlpha(c) || isDigit(c))  =>  getId(ident + c, cs)
    case cs                                     =>  (keyword(ident), cs)
  }

  private def getNum(num: Int, l: List[Char]): (TOK, List[Char]) = l match {
    case c :: cs if isDigit(c)    =>  getNum(10*num + c.toInt, cs)
    case cs                       =>  (NUMTOK(num), cs)
  }

  private def getTok(l: List[Char]): (TOK, List[Char]) = l match {
    case Nil => (EOF(), List())
    case c :: cs if isWhite(c) => getTok(cs)
    case c :: cs if isAlpha(c) => getId(c.toString, cs)
    case c :: cs if isDigit(c) => getNum(c.toString.toInt, cs)
    case '(' :: cs => (LPARENTOK(), cs)
    case ')' :: cs => (RPARENT(), cs)
    case '=' :: cs => (EQUALTOK(), cs)
    case '-' :: cs => (EQUALTOK(), cs)
    case '#' :: cs =>
      def skipLine(x: List[Char]): (TOK, List[Char]) = x match {
        case Nil => (EOF(), List())
        case '\n' :: cs1 => getTok(cs1)
        case _ :: cs1 => skipLine(cs1)
      }
      skipLine(cs)
    case c :: cs => {
      println("Skipping illegal character: " + c)
      getTok(cs)
    }
  }

  private def tokenize(cs: List[Char]): List[TOK] = {
    def getToks(toks: List[TOK], cs: List[Char]): List[TOK] = getTok(cs) match {
      case (EOF(), cs) => (EOF() :: toks).reverse
      case (tok, cs) => getToks(tok :: toks, cs)
    }
    getToks(List(), cs)
  }

  def lexerStr(sourceCode: String): List[TOK] = sourceCode |> explode |> tokenize

  def lexerFile(fileName: String): List[TOK] = scala.io.Source.fromFile(fileName).mkString |> lexerStr
}
