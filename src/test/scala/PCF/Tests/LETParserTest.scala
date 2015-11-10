/**
 * Created by nperez on 11/10/15.
 */

package PCF.Tests

import PCF.Parser.AST.{ID, FUNC, APP, ERROR}
import PCF.Parser.Parser
import org.scalatest.{ShouldMatchers, FlatSpec}

class LETParserTest extends FlatSpec with ShouldMatchers {
  "Parser" should "return ERROR when LET has not body" in {
    val sourceCode = "let x = "

    val ast = Parser.parseStr(sourceCode)

    ast should be (ERROR("Expecting LET body"))
  }

  it should "return FUN TERM when body is present" in {
    val sourceCode = "let x = a in b" // translated to (fun x -> b)(a)

    val ast = Parser.parseStr(sourceCode)

    ast should be (APP(FUNC("x", ID("b")), ID("a")))
  }

  it should "return ERROR when IN in missing" in {
    val sourceCode = "let x = a b" // translated to (fun x -> b)(a)

    val ast = Parser.parseStr(sourceCode)

    ast should be (ERROR("LET requires IN"))
  }

  it should "return ERROR when inner expression cannot be parsed" in {
    val sourceCode = "let x = a in ;" // translated to (fun x -> b)(a)

    val ast = Parser.parseStr(sourceCode)

    ast should be (ERROR("LET requires valid expression after IN"))
  }

  it should "return ERROR when '=' is missing" in {
    val sourceCode = "let x  a b" // translated to (fun x -> b)(a)

    val ast = Parser.parseStr(sourceCode)

    ast should be (ERROR("LET requires '=' after variable"))
  }

  it should "return ERROR when varialble is missing" in {
    val sourceCode = "let =  a in b" // translated to (fun x -> b)(a)

    val ast = Parser.parseStr(sourceCode)

    ast should be (ERROR("LET requires variable definition"))
  }
}
