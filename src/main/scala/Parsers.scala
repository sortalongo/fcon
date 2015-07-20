package co.sortalon.fcon

import scala.util.parsing.combinator.RegexParsers
import scala.util.{Try, Success => Return, Failure => Throw}

object AST {
  trait Stage

  sealed trait Node[S <: Stage] {
    def stage: S
    def children: List[Node[S]]
    def copyMap[T <: Stage](stage2: T)(f: Node[S] => Node[T]): Node[T]
  }

  sealed trait Terminal[S <: Stage] extends Node[S] {
    def children = Nil
  }
  case class Sym[S <: Stage](s: String)(implicit val stage: S) extends Terminal[S] {
    def copyMap[T <: Stage](stage2: T)(f: Node[S] => Node[T]) = copy()(stage2)
  }
  case class Str[S <: Stage](s: String)(implicit val stage: S) extends Terminal[S] {
    def copyMap[T <: Stage](stage2: T)(f: Node[S] => Node[T]) = copy()(stage2)
  }

  case class Lst[S <: Stage](
    elems: List[Node[S]]
  )(implicit val stage: S
  ) extends Node[S] {
    def children = elems
    def copyMap[T <: Stage](stage2: T)(f: Node[S] => Node[T]) =
      copy(elems map f)(stage2)
  }

  case class Pair[S <: Stage](key: Str[S], value: Node[S]) extends Node[S] {
    def children = List(key, value)
    def copyMap[T <: Stage](stage2: T)(f: Node[S] => Node[T]) =
      copy(f(key), f(value))(stage2)
  }
  case class Dict[S <: Stage](
    pairs: List[Pair[S]]
  )(implicit val stage: S
  ) extends Node[S] {
    def children = pairs
    def copyMap[T <: Stage](stage2: T)(f: Node[S] => Node[T]) =
      copy(pairs map f)(stage2)
  }

  case class Func[S <: Stage](
    args: List[Str[S]],
    body: Node[S]
  )(implicit val stage: S
  ) extends Node[S] {
    def children = body :: args
    def copyMap[T <: Stage](stage2: T)(f: Node[S] => Node[T]) =
      copy(args map f, f(body))(stage2)
  }

  case class Merged[S <: Stage](
    nodes: List[Node[S]]
  )(implicit val stage: S
  ) extends Node[S] {
    def children = nodes
    def copyMap[T <: Stage](stage2: T)(f: Node[S] => Node[T]) =
      copy(nodes map f)(stage2)
  }
}

object Parsers {
  val reservedChars = """{}\(\)\[\]`,:."""
  val quoteChars = "\"'"

  val s = "\\s*".r
  val newline = "[\r\n]+".r
  val unreserved = s"""[^${reservedChars}${quoteChars}\r\n]+""".r

  implicit object Parsed extends AST.Stage
  type P = Parsed.type

  class ParsingError(m: String) extends Exception(m)
}

class Parsers extends RegexParsers {
  import Parsers._
  import AST._

  override val skipWhitespace = false

  def apply(s: String): Try[Node[P]] = parseAll(fcon, s) match {
    case Success(result, _) => Return(result)
    case failure => Throw(new ParsingError(s"Error parsing string: $failure"))
  }

  def fcon: Parser[Node[P]] = s ~> expr <~ s

  def expr: Parser[Node[P]] = rep1sep(atom | parens, s) ^^ {
    case List(single) => single
    case multi @ List(_*) => Merged(multi)
  }
  def parens: Parser[Node[P]] = "(" ~> s ~> expr <~ s <~ ")"

  def atom: Parser[Node[P]] = list | dict | func | string | sym

  def sym: Parser[Sym[P]] = "`" ~> "[^`]*".r <~ "`" ^^ { Sym(_) }
  def list: Parser[Lst[P]] = "[" ~> repsep(s ~> expr <~ s, implComma) <~ s <~ "]" ^^ { Lst(_) }
  def dict: Parser[Dict[P]] = "{" ~> repsep(s ~> pair <~ s, implComma) <~ s <~ "}" ^^ { Dict(_) }
  def pair: Parser[Pair[P]] = string ~ (s ~> ":" ~> s ~> expr) ^^ {
    case key ~ value => Pair(key, value)
  }
  def func: Parser[Func[P]] =
    ( "(" ~> rep1sep( s ~> string, ",") <~ s ) ~ ( ":" ~> s ~> expr <~ s <~ ")" ) ^^ {
      case args ~ body => Func(args, body)
    }

  def implComma: Parser[String] = "," | "\n"
  def string: Parser[Str[P]] =
    ( "\"" ~> "[^\"]".r <~ "\"" | unreserved ) ^^ { s: String => Str(s.trim) }
}
