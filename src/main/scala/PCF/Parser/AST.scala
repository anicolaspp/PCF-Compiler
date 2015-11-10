/**
  * Created by nperez on 11/10/15.
 */
// This is the grammar of PCF

package PCF.Parser.AST

// Exp ::=  x | n                               case (1)
//            | true                            case (2)
//            | false                           case (3)
//            | succ                            case (4)
//            | pred                            case (5)
//            | iszero                          case (6)
//            | if Exps then Exps else Exps     case (7)
//            | fun x -> Exps                   case (8)
//            | rec x -> Exps                   case (9)
//            | (Exps)                          case (10)
//            | let x = Exps in Exps            case (11)
//            |
// Exps ::= Exps Exp | Exp
//
// We resolve the ambiguity in the grammar by making concatenation
// (i.e. function application) bind tighter than if, fun, rec, and let.

abstract class TERM()

case class ID(val id: String) extends TERM
case class NUM(val value: Int) extends TERM
case class BOOL(val value: Boolean) extends TERM
case class SUCC() extends TERM
case class PRED() extends TERM
case class ISZERO() extends TERM
case class IF(condition: TERM, Then: TERM, Else: TERM) extends TERM
case class APP(function: TERM, value: TERM) extends TERM
case class FUNC(name: String, body: TERM) extends TERM
case class REC(name: String, body: TERM) extends TERM
case class ERROR(reason: String) extends TERM


