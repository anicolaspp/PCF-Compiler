/**
 * Created by nperez on 11/9/15.
 */


import PCF.Parser.Parser
import PCF.Tokenizer.Lexer

object App {
  def main(args: Array[String]) {
    val source = "suc suc suc 5"

   val ast = Parser.parseStr(source)

    println(ast)
  }
}
