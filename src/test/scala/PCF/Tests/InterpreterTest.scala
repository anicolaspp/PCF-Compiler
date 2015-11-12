package PCF.Tests

/**
 * Created by nperez on 11/11/15.
 */
class InterpreterTest extends FlatSpec with ShouldMatchers {
  "Interpreter" should "return ERROR when unbound indentifier" in {

    val ast = ID("x")

    val result = Interpreter.eval(ast)

    result should be (ERROR("Unbound identifier"))
  }

  it should "return ERROR when REC has not FUN body" in {

    Interpreter.eval(REC("f", APP(ID("x"), ID("x")))) should be (ERROR("Expecting FUN body"))
    Interpreter.eval(REC("f", BOOL(true))) should be (ERROR("Expecting FUN body"))
    Interpreter.eval(REC("f", ERROR("asdf"))) should be (ERROR("Expecting FUN body"))
    Interpreter.eval(REC("f", IF(ID("a"),ID("a"),ID("a")))) should be (ERROR("Expecting FUN body"))
    Interpreter.eval(REC("f", ISZERO())) should be (ERROR("Expecting FUN body"))
    Interpreter.eval(REC("f", NUM(5))) should be (ERROR("Expecting FUN body"))
    Interpreter.eval(REC("f", PRED())) should be (ERROR("Expecting FUN body"))
    Interpreter.eval(REC("f", SUCC())) should be (ERROR("Expecting FUN body"))
  }

  it should "return NUM when apply  NUM" in {
    val ast =  NUM(5)

    Interpreter.eval(ast) should be (NUM(5))
  }

  it should "return 6 when apply SUCC NUM(5)" in {
    val ast = APP(SUCC(), NUM(5))

    Interpreter.eval(ast) should be (NUM(6))
  }

  it should "return 5 when apply PRED NUM(6)" in {
    val ast = APP(PRED(), NUM(6))

    Interpreter.eval(ast) should be (NUM(5))
  }
}
