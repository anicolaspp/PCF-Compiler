/**
 * Created by nperez on 11/9/15.
 */


import PCF.Interpreter._
import PCF.Parser.Parser
import PCF.Tokenizer.Lexer

object App {
  def main(args: Array[String]) {
    val source = "let s = 5 0 in rec i -> " +
      "func x -> fun y -> if iszero y then x else i (succ x) (pred y)"

   val ast = Parser.parseStr(source)

    val result = Interpreter.eval(ast)

    println(result)
  }
}
