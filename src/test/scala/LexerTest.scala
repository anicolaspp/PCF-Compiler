/**
  * Created by nperez on 11/9/15.
 */

package PCF.Tests

import PCF.Tokenizer._
import org.scalatest.{ShouldMatchers, FlatSpec}



class LexerTest extends FlatSpec with ShouldMatchers {

  "Lexer" should "return no tokens with empty source code" in {
      val sourceCode = ""

      val tokens = Lexer.lexerStr(sourceCode)

      tokens should be (List(EOF()))
  }

  it should "return IDTOKEN with ID Name" in {
    val sourceCode = "x"

    val tokens = Lexer.lexerStr(sourceCode)
    val idTok = tokens(0).asInstanceOf[IDTOK]

    tokens should be (List(IDTOK("x"), EOF()))
    idTok.id should be ("x")
  }

  it should "return NUNTOK with value" in {
    val sourceCode = "5"

    val tokens = Lexer.lexerStr(sourceCode)
    val numTok = tokens(0).asInstanceOf[NUMTOK]

    tokens should be (List(NUMTOK(5), EOF()))
    numTok.value should be (5)
  }

  it should "return TRUETOK when true" in {
    val sourceCode = "true"

    val tokens = Lexer.lexerStr(sourceCode)

    tokens should be (List(TRUETOK(), EOF()))
  }

  it should "return FALSETOK when false" in {
    val sourceCode = "false"

    val tokens = Lexer.lexerStr(sourceCode)

    tokens should be (List(FALSETOK(), EOF()))
  }

  it should "return SUCCTOK with succ" in {
    val sourceCode = "succ"

    val tokens = Lexer.lexerStr(sourceCode)

    tokens should be (List(SUCCTOK(), EOF()))
  }

  it should "return PREDTOK with pred" in {
    val sourceCode = "pred"

    val tokens = Lexer.lexerStr(sourceCode)

    tokens should be (List(PREDTOK(), EOF()))
  }

  it should "return ISZERO with iszero" in {
    val sourceCode = "iszero 0"

    val tokens = Lexer.lexerStr(sourceCode)

    tokens should be (List(ISZERO(), NUMTOK(0), EOF()))
  }

  it should "return IFTOK when if else then" in {
    val sourceCode = "if x then x else x"

    val tokens = Lexer.lexerStr(sourceCode)

    tokens should be (List(IFTOK(),IDTOK("x"),THENTOK(), IDTOK("x"), ELSETOK(), IDTOK("x"), EOF()))
  }

  it should "return FUNTOK when fun" in {
    val sourceCode = "fun x -> y"

    val tokens = Lexer.lexerStr(sourceCode)

    tokens should be (List(FUNTOK(), IDTOK("x"), EQUALTOK(), IDTOK("y"), EOF()))
  }

  it should "return RECTOK when rec" in {
    val sourceCode = "rec x -> x"

    val tokens = Lexer.lexerStr(sourceCode)

    tokens should be (List(RECTOK(), IDTOK("x"), EQUALTOK(), IDTOK("x"), EOF()))
  }

  it should "return L-R PARENTOK when (...)" in {
    val sourceCode = "(x(y)z)"

    val tokens = Lexer.lexerStr(sourceCode)

    tokens should be (List(LPARENTOK(), IDTOK("x"), LPARENTOK(), IDTOK("y"), RPARENT(), IDTOK("z"), RPARENT(), EOF()))
  }

  it should "LET when asign" in {
    val sourceCode = "let x = 5"

    val tokens = Lexer.lexerStr(sourceCode)

    tokens should be (List(LETTOK(), IDTOK("x"), EQUALTOK(), NUMTOK(5), EOF()))
  }

  it should "not return a non-token" in {
    val sourceCode = "let x = fun y -> if (a) then ((b)) else c"

    val tokens = Lexer.lexerStr(sourceCode)

    tokens.foreach(t => t.isInstanceOf[TOK] should be (true))
  }

  it should "igone commented line" in {
    val sourceCode = "rec x -> x\n#as;lkdfja;sldfkas\nsucc 5"

    val tokens = Lexer.lexerStr(sourceCode)

    tokens should be (List(RECTOK(), IDTOK("x"), EQUALTOK(), IDTOK("x"), SUCCTOK(), NUMTOK(5) ,EOF()))
  }
}
