/**
  * Created by nperez on 11/10/15.
 */


package PCF.Parser.AST

abstract class TERM()

abstract class RESULT_TERM extends TERM

case class ID(val id: String) extends TERM
case class NUM(val value: Int) extends RESULT_TERM
case class BOOL(val value: Boolean) extends RESULT_TERM
case class SUCC() extends TERM
case class PRED() extends TERM
case class ISZERO() extends TERM
case class IF(condition: TERM, Then: TERM, Else: TERM) extends TERM
case class APP(function: TERM, value: TERM) extends TERM
case class FUNC(name: String, body: TERM) extends TERM
case class REC(name: String, body: TERM) extends TERM
case class ERROR(reason: String = "") extends RESULT_TERM


