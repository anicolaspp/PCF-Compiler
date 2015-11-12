/**
  * Created by nperez on 11/11/15.
 */

package PCF.Interpreter

import PCF.Parser.AST._

object Interpreter {

  def eval(ast: TERM): TERM = ast match {

    case APP(func, e) => (eval(func), eval(e)) match {
      case (ERROR(reason), _) => ERROR(reason)
      case (_, ERROR(reason)) => ERROR(reason)
      case (SUCC(), NUM(x)) => NUM(x + 1)
      case (SUCC(), _)      => ERROR("SUCC expects a NUM")
      case (PRED(), NUM(0)) => NUM(0)
      case (PRED(), NUM(x)) => NUM(x - 1)
      case (PRED(), _)      => ERROR("PRED expects a NUM")
      case (ISZERO(), NUM(0)) => BOOL(true)
      case (ISZERO(), NUM(n)) => BOOL(false)
    }

    case ERROR(reason)=>ERROR(reason)
    case ISZERO()=>ISZERO()
    case BOOL(v)  => BOOL(v)
    case SUCC() => SUCC()
    case PRED() => PRED()
    case NUM(v) => NUM(v)
    case REC(name, _) => ERROR("Expecting FUN body")
    case ID("x") => ERROR("Unbound identifier")
  }
}
