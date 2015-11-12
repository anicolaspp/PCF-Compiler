/**
  * Created by nperez on 11/11/15.
 */

package PCF.Interpreter

import PCF.Parser.AST._

object Interpreter {

  private def subst(e: TERM, x: String, t: TERM): TERM = e match {
    case ID(s) if s == x  =>  t
    case ID(s) if s != x  =>  ID(s)
    case IF(cond, e1, e2) =>  IF (subst (cond, x, t), subst (e1, x, t), subst (e2, x, t))
    case APP(func, s)     =>  APP(subst (func, x, t), subst (s, x, t))
    case FUNC(s, term)    =>  if (s == x) FUNC(s, term) else FUNC(s, subst (term, x, t))
    case REC(s, term)     =>  if (s == x) REC(s, term) else REC(s, subst (term, x, t))
    case any              =>  any
  }

  def eval(ast: TERM): TERM = ast match {

    case IF(guard,thenExp,elseExp)  => (eval(guard), thenExp, elseExp) match{
      case (BOOL(true), e1, _)  =>  eval(e1)
      case (BOOL(false), _, e2) =>  eval(e2)
      case (_,_,_)              =>  ERROR("IF requires BOOL guard")
    }


    case APP(func, e) => (eval(func), eval(e)) match {
      case (ERROR(reason), _) => ERROR(reason)
      case (_, ERROR(reason)) => ERROR(reason)
      case (SUCC(), NUM(x))   => NUM(x + 1)
      case (SUCC(), _)        => ERROR("SUCC expects a NUM")
      case (PRED(), NUM(0))   => NUM(0)
      case (PRED(), NUM(x))   => NUM(x - 1)
      case (PRED(), _)        => ERROR("PRED expects a NUM")
      case (ISZERO(), NUM(0)) => BOOL(true)
      case (ISZERO(), NUM(n)) => BOOL(false)
      case (FUNC(v, exp), e2) => eval(subst(exp, v, eval(e2)))
      case (other, _)         => ERROR("Cannot Apply this function")
    }


    case REC(name, FUNC(t, exp)) => eval ( FUNC(t, subst(exp, name, ( REC(name, FUNC(t, exp))))))
    case REC(name, _) => ERROR("Expecting FUN body")
    case ID("x") => ERROR("Unbound identifier")
    case any    => any
  }
}
