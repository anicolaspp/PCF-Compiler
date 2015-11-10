/**
 * Created by nperez on 11/10/15.
 */

package PCF.Tests


import PCF.Parser.AST._
import PCF.Parser.Parser
import org.scalatest.{ShouldMatchers, FlatSpec}

class BasicParserTest extends FlatSpec with ShouldMatchers{

  "Parser" should "return ID TERM case 1" in {
    val id = "x"

    val ast = Parser.parseStr(id)

    ast should be (ID("x"))
  }

  it should "return NUM TERM case 1.1" in {
    val n = "5"

    val ast = Parser.parseStr(n)

    ast should be (NUM(5))
  }

  it should "return TRUE TERM case 2" in {
    val n = "true"

    val ast = Parser.parseStr(n)

    ast should be (BOOL(true))
  }

  it should "return FALSE TERM case 3" in {
    val n = "false"

    val ast = Parser.parseStr(n)

    ast should be (BOOL(false))
  }

  it should "return SUCC TERM case 4" in {
    val n = "succ"

    val ast = Parser.parseStr(n)

    ast should be (SUCC())
  }

  it should "return PRED TERM case 5" in {
    val n = "pred"

    val ast = Parser.parseStr(n)

    ast should be (PRED())
  }

  it should "return ISZERO TERM case 6" in {
    val n = "iszero"

    val ast = Parser.parseStr(n)

    ast should be (ISZERO())
  }
}

class IFPaserTest extends FlatSpec with ShouldMatchers{
  "Parser" should "return ERROR when IF (NONTERM)" in {
    val sourceCode = "if ;"

    val ast = Parser.parseStr(sourceCode)

    ast should be (ERROR("Expected Expression but found: EOF()"))
  }

  it should "return ERROR when IF does not has THEN" in {
      val sourceCode = "if x else"

      val ast = Parser.parseStr(sourceCode)

      ast should be (ERROR("IF requires an TERM as guard"))
  }



  it should "return ERROR when IF THEN (NONTERM)" in {
    val sourceCode = "if x then ;"

    val ast = Parser.parseStr(sourceCode)

    ast should be (ERROR("IF requires an TERM after THEN"))
  }

  it should "return ERROR when IF THEN and missing ELSE" in {
    val sourceCode = "if x then 5"

    val ast = Parser.parseStr(sourceCode)

    ast should be (ERROR("IF requires an ELSE"))
  }

  it should "return ERROR when IF THEN ELSE (NONTERM)" in {
    val sourceCode = "if x then 5 else ;"

    val ast = Parser.parseStr(sourceCode)

    ast should be (ERROR("IF requires an TERM after ELSE"))
  }

  it should "return IF THEN ELSE TERM" in {
    val sourceCode = "if x then 5 else a"

    val ast = Parser.parseStr(sourceCode)

    ast should be(IF(ID("x"), NUM(5), ID("a")))
  }

  it should "return ERROR valid guard but nothing else" in {
    val sourceCode = "if a"

    val ast = Parser.parseStr(sourceCode)
  }
}
