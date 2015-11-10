package PCF.Tests

import PCF.Parser.AST._
import PCF.Parser.Parser
import org.scalatest._

/**
 * Created by nperez on 11/10/15.
 */
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
