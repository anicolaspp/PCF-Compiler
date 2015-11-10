/**
 * Created by nperez on 11/9/15.
 */


import PCF.Tokenizer.Lexer

object App {
  def main(args: Array[String]) {
    val source = "5"

    Lexer.lexerStr(source).map(println)
  }
}
