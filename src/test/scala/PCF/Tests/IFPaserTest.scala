/**
 * Created by nperez on 11/10/15.
 */

package PCF.Tests

import PCF.Parser.AST.{ERROR, ID, IF, NUM}
import PCF.Parser.Parser
import org.scalatest._

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
