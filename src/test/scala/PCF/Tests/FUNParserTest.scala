/**
 * Created by nperez on 11/10/15.
 */

package PCF.Tests

import PCF.Parser.AST._
import PCF.Parser.Parser
import org.scalatest.{FlatSpec, ShouldMatchers}


class FUNParserTest extends FlatSpec with ShouldMatchers {
  "Parser" should "return ERROR when FUN has not body" in {
    val sourceCode = "fun x -> "

    val ast = Parser.parseStr(sourceCode)

    ast should be (ERROR("Expecting FUN body"))
  }

  it should "return FUN TERM when body is present" in {
    val sourceCode = "fun x -> a"

    val ast = Parser.parseStr(sourceCode)

    ast should be (FUNC("x", ID("a")))
  }

  it should "return ERROR when ARROW is missing" in {
    val sourceCode = "fun x b a"

    val ast = Parser.parseStr(sourceCode)

    ast should be (ERROR("Expected '->' after FUN"))
  }

  it should "return ERROR when ID is missing" in {
    val sourceCode = "fun -> b"

    val ast = Parser.parseStr(sourceCode)

    ast should be (ERROR("Expecting identifier after FUN"))
  }
}



