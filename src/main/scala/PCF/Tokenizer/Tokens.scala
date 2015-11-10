/**
 * Created by nperez on 11/9/15.
 */

package PCF.Tokenizer

abstract class TOK()

case class IDTOK(val id: String) extends TOK
case class NUMTOK(val value: Int)extends TOK
case class TRUETOK()extends TOK
case class FALSETOK()extends TOK
case class SUCCTOK()extends TOK
case class PREDTOK()extends TOK
case class ISZEROTOK()extends TOK
case class IFTOK()extends TOK
case class THENTOK()extends TOK
case class ELSETOK()extends TOK
case class FUNTOK()extends TOK
case class RECTOK()extends TOK
case class ARROWTOK()extends TOK
case class LPARENTOK()extends TOK
case class RPARENT()extends TOK
case class LETTOK()extends TOK
case class EQUALTOK()extends TOK
case class INTOK()extends TOK
case class EOF() extends TOK





