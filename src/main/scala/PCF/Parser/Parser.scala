/**
  * Created by nperez on 11/10/15.
 */

package PCF.Parser

// This is the grammar of PCF

// Exp ::=  x | n
//            | true
//            | false
//            | succ
//            | pred
//            | iszero
//            | if Exps then Exps else Exps
//            | fun x -> Exps
//            | rec x -> Exps
//            | (Exps)
//            | let x = Exps in Exps
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




class Parser {

}
