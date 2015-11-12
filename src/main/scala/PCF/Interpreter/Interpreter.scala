/**
  * Created by nperez on 11/11/15.
 */

package PCF.Interpreter

import PCF.Parser.AST._

object Interpreter {

  def eval(ast: TERM): TERM = ast match {

    case APP(func, e) => (eval(func), eval(e)) match {
      case (SUCC(), NUM(x)) => NUM(x + 1)
      case (PRED(), NUM(x)) => NUM(x - 1)
    }

    case SUCC() => SUCC()
    case PRED() => PRED()
    case NUM(v) => NUM(v)
    case REC(name, _) => ERROR("Expecting FUN body")
    case ID("x") => ERROR("Unbound identifier")
  }
}