/**
 * Created by nperez on 11/9/15.
 */


import PCF.Interpreter._
import PCF.Parser.Parser
import PCF.Tokenizer.Lexer

object App {
  def main(args: Array[String]) {
    val source = "let "

   val ast = Parser.parseStr(source)

    val result = Interpreter.eval(ast)

    println(result)
  }
}
