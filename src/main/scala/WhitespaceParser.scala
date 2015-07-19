package co.sortalon.fcon

import scala.util.parsing.combinator.RegexParsers
import scala.util.{Try, Success => Return, Failure => Throw}

object AST {
  sealed trait Node
  case class Sym(s: String) extends Node
  case class Str(s: String) extends Node
  case class Lst(elems: List[Node]) extends Node
  case class Dict(pairs: List[(Str, Node)]) extends Node
  case class Func(args: List[Str], body: Node*) extends Node
  case class Merged(nodes: List[Node]) extends Node
}

object Parsers {
  val reservedChars = """{}\(\)\[\]`,:."""
  val quoteChars = "\"'"

  val w = "\\w*".r
  val newline = "[\r\n]+".r
  val unreserved = s"""[^${reservedChars}${quoteChars}\r\n]+""".r

  class ParsingError(m: String) extends Exception(m)
}

class Parsers extends RegexParsers {
  import Parsers._
  import AST._

  override val skipWhitespace = false

  def apply(s: String): Try[Node] = parseAll(fcon, s) match {
    case Success(result, _) => Return(result)
    case failure => Throw(new ParsingError(s"Error parsing string: $failure"))
  }

  def fcon: Parser[Node] = w ~> expr <~ w

  def expr: Parser[Node] = rep1sep(atom | parens, w) ^^ {
    case List(single) => single
    case multi @ List(_*) => Merged(multi)
  }
  def parens: Parser[Node] = "(" ~> w ~> expr <~ w <~ ")"

  def atom: Parser[Node] = list | dict | func | string

  def sym: Parser[Sym] = "`" ~> "[^`]*".r <~ "`" ^^ { Sym(_) }
  def list: Parser[Lst] = "[" ~> repsep(w ~> expr <~ w, implComma) <~ w <~ "]" ^^ { Lst(_) }
  def dict: Parser[Dict] = "{" ~> repsep(w ~> pair <~ w, implComma) <~ w <~ "}" ^^ { Dict(_) }
  def pair: Parser[(Str, Node)] = string ~ (w ~> ":" ~> w ~> expr) ^^ {
    case key ~ value => (key, value)
  }
  def func: Parser[Func] =
    ( "(" ~> rep1sep( w ~> string, ",") <~ w ) ~ ( ":" ~> w ~> expr <~ w <~ ")" ) ^^ {
      case args ~ body => Func(args, body)
    }


  def implComma: Parser[String] = "," | "\n"
  def string: Parser[Str] =
    ( "\"" ~> "[^\"]".r <~ "\"" | unreserved ) ^^ { s: String => Str(s.trim) }
}
