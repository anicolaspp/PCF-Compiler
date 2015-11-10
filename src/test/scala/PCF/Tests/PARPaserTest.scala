/**
 * Created by nperez on 11/10/15.
 */

package PCF.Tests

import PCF.Parser.AST._
import PCF.Parser.Parser
import org.scalatest.{ShouldMatchers, FlatSpec}

class PARPaserTest extends FlatSpec with ShouldMatchers {
  "Parser" should "return ERROR when ( NONTERM" in {
    val sourceCode = "(;"

    val ast = Parser.parseStr(sourceCode)

    ast should be (ERROR("After '(' an valid expression has to follow"))
  }

  it should "return ERROR when '('TERM but missing ')'" in {
    val sourceCode = "(a"

    val ast = Parser.parseStr(sourceCode)

    ast should be (ERROR("After ')' expresion"))
  }

  it should "return Expresion when (Expression)" in {
    val sourceCode = "(a)"

    val ast = Parser.parseStr(sourceCode)

    ast should be (ID("a"))
  }
}
