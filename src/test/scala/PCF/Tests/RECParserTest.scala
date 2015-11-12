package PCF.Tests

import PCF.Interpreter.Interpreter
import PCF.Parser.AST._
import PCF.Parser.Parser
import org.scalatest.{ShouldMatchers, FlatSpec}

/**
 * Created by nperez on 11/10/15.
 */
class RECParserTest extends FlatSpec with ShouldMatchers {
  "Parser" should "return ERROR when REC has not body" in {
    val sourceCode = "rec x -> "

    val ast = Parser.parseStr(sourceCode)

    ast should be (ERROR("Expecting REC body"))
  }

  it should "return REC TERM when body is present" in {
    val sourceCode = "rec x -> a"

    val ast = Parser.parseStr(sourceCode)

    ast should be(REC("x", ID("a")))
  }

  it should "return ERROR when ARROW is missing" in {
    val sourceCode = "rec x b a"

    val ast = Parser.parseStr(sourceCode)

    ast should be (ERROR("Expected '->' after REC"))
  }


  it should "return ERROR when ID is missing" in {
    val sourceCode = "rec -> b"

    val ast = Parser.parseStr(sourceCode)

    ast should be (ERROR("Expecting identifier after REC"))
  }
}


