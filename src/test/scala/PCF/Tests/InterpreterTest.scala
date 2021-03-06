/**
 * Created by nperez on 11/11/15.
 */

package PCF.Tests

import PCF.Interpreter.Interpreter
import PCF.Parser.AST._
import PCF.Parser.Parser
import PCF.Pipes.IPipe._
import org.scalatest.{ShouldMatchers, FlatSpec}

class InterpreterTest extends FlatSpec with ShouldMatchers {
  "Interpreter" should "return ERROR when unbound indentifier" in {

    val ast = ID("x")

    val result = Interpreter.eval(ast)

    result should be(ERROR("Unbound identifier"))
  }

  it should "return ERROR when REC has not FUN body" in {

    Interpreter.eval(REC("f", APP(ID("x"), ID("x")))) should be(ERROR("Expecting FUN body"))
    Interpreter.eval(REC("f", BOOL(true))) should be(ERROR("Expecting FUN body"))
    Interpreter.eval(REC("f", ERROR("asdf"))) should be(ERROR("Expecting FUN body"))
    Interpreter.eval(REC("f", IF(ID("a"), ID("a"), ID("a")))) should be(ERROR("Expecting FUN body"))
    Interpreter.eval(REC("f", ISZERO())) should be(ERROR("Expecting FUN body"))
    Interpreter.eval(REC("f", NUM(5))) should be(ERROR("Expecting FUN body"))
    Interpreter.eval(REC("f", PRED())) should be(ERROR("Expecting FUN body"))
    Interpreter.eval(REC("f", SUCC())) should be(ERROR("Expecting FUN body"))
  }

  it should "return NUM when apply  NUM" in {
    val ast = NUM(5)

    Interpreter.eval(ast) should be(NUM(5))
  }

  it should "return 6 when apply SUCC NUM(5)" in {
    val ast = APP(SUCC(), NUM(5))

    Interpreter.eval(ast) should be(NUM(6))
  }

  it should "return ERROR when apply SUCC to NON NUM" in {
    val ast = APP(SUCC(), BOOL(true))

    Interpreter.eval(ast) should be(ERROR("SUCC expects a NUM"))
  }

  it should "return 5 when apply PRED NUM(6)" in {
    val ast = APP(PRED(), NUM(6))

    Interpreter.eval(ast) should be(NUM(5))
  }

  it should "return ERROR when apply PRED to NON NUM" in {
    val ast = APP(PRED(), BOOL(true))

    Interpreter.eval(ast) should be(ERROR("PRED expects a NUM"))
  }

  it should "return NUM(0) when apply PRED NUM(0)" in {
    val ast = APP(PRED(), NUM(0))

    Interpreter.eval(ast) should be(NUM(0))
  }

  it should "return BOOL(true) when apply ISZERO NUM(0)" in {
    val ast = APP(ISZERO(), NUM(0))

    Interpreter.eval(ast) should be(BOOL(true))
  }

  it should "return BOOL(false) when apply ISZERO NUM(1)" in {
    val ast = APP(ISZERO(), NUM(1))

    Interpreter.eval(ast) should be(BOOL(false))
  }

  it should "return ERROR when APP has not FUN" in {
    val ast = APP(ERROR(), NUM(1))

    Interpreter.eval(ast) should be(ERROR())
  }

  it should "return ERROR when APP has not arguments" in {
    val ast = APP(SUCC(), ERROR())

    Interpreter.eval(ast) should be(ERROR())
  }

  it should "return NUM(5) when IF guard is BOOL(true)" in {
    val ast = IF(BOOL(true), NUM(5), ID("x"))

    Interpreter.eval(ast) should be(NUM(5))
  }

  it should "return NUM(6) when IF guard is BOOL(false)" in {
    val ast = IF(BOOL(false), NUM(5), NUM(6))

    Interpreter.eval(ast) should be(NUM(6))
  }

  it should "return ERROR en IF guard is not BOOL" in {
    val ast = IF(NUM(5), ID("x"), ID("x"))

    Interpreter.eval(ast) should be(ERROR("IF requires BOOL guard"))
  }

  it should "recursively eval expressions on IF" in {

    Interpreter.eval(IF(BOOL(false), NUM(0), APP(PRED(), NUM(5)))) should be(NUM(4))
    Interpreter.eval(IF(BOOL(true), APP(PRED(), NUM(5)), NUM(0))) should be(NUM(4))
  }

  it should "apply FUN recursively" in {
    val source = "let x = 3 in if true then succ succ x else 0"

    val ast = Parser.parseStr(source)

    Interpreter.eval(ast) should be (NUM(5))
  }
}

