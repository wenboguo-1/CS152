package context
import scala.util.parsing.combinator._
class SyntaxException(val result: Parsers#Failure = null) extends JediException("Syntax error")