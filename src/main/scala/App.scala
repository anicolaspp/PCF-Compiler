/**
 * Created by nperez on 11/9/15.
 */


import PCF.Parser.Parser
import PCF.Tokenizer.Lexer

object App {
  def main(args: Array[String]) {
    val source = "fun x -> "

   val ast = Parser.parseStr(source)

    println(ast)
  }
}
