package co.sortalon.fcon

import Parsers.{P, Parsed}
import scala.util.parsing.combinator.RegexParsers
import scala.util.{Try, Success => Return, Failure => Throw}

object AST {
  trait Stage

  sealed trait Node[S <: Stage] {
    def stage: S
  }

  sealed trait Terminal[S <: Stage] extends Node[S]

  sealed trait Sym extends Terminal[P] {
    val stage = Parsed
  }
  object Sym {
    def apply(s: String*) = s match {
      case Seq(atom) => Atom(atom)
      case _ => sub(Atom(s.last), s.init)
    }

    def sub(sym: Sym, prefix: Seq[String]) = prefix.foldRight(sym) {
      case (part, chain) => Scoped(Atom(part), chain)
    }

    case class Atom(s: String) extends Sym
    case class Scoped(s1: Atom, ss: Sym) extends Sym
  }

  case class Str[S <: Stage](s: String)(implicit val stage: S) extends Terminal[S]

  case class Lst[S <: Stage](
    elems: List[Node[S]]
  )(implicit val stage: S
  ) extends Node[S]

  case class Pair[S <: Stage](
    key: Sym.Atom,
    value: Node[S]
  )(implicit val stage: S
  ) extends Node[S]

  case class Dict[S <: Stage](
    pairs: List[Pair[S]]
  )(implicit val stage: S
  ) extends Node[S]

  sealed trait Func[S <: Stage, T <: Stage] extends Node[S]
  object Func {
    case class Base[S <: Stage, T <: Stage](
      arg: Sym.Atom,
      // a function's body isn't resolved until after the arg is substituted
      // so, its stage lags behind the rest of the func
      body: Node[T]
    )(implicit val stage: S
    ) extends Func[S, T]

    case class Deferred[S <: Stage, T <: Stage](
      prior: Node[S],
      f: Func[S, T]
    ) extends Func[S, T] {
      def stage = f.stage
    }
  }


  case class Merged[S <: Stage](
    nodes: List[Node[S]]
  )(implicit val stage: S
  ) extends Node[S]
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

  def symatom: Parser[Sym.Atom] =
    optEnclosed("`") ^^ { Sym.Atom(_) }
  def sym: Parser[Sym] =
    "`" ~> rep1sep("[^`]+".r, ".") <~ "`" ^^ { Sym(_: _*) }

  def list: Parser[Lst[P]] =
    "[" ~> repsep(s ~> expr <~ s, implComma) <~ s <~ "]" ^^ { Lst(_) }
  def dict: Parser[Dict[P]] =
    "{" ~> repsep(s ~> pair <~ s, implComma) <~ s <~ "}" ^^ { Dict(_) }
  def pair: Parser[Pair[P]] = string ~ (s ~> ":" ~> s ~> expr) ^^ {
    case Str(key) ~ value => Pair(Sym.Atom(key), value)
  }
  def func: Parser[Func[P, P]] =
    ( "(" ~> rep1sep( s ~> symatom, ",") <~ s ) ~ ( ":" ~> s ~> expr <~ s <~ ")" ) ^^ {
      case args ~ body =>
        args.init.foldRight(Func.Base(args.last, body)) {
          case (arg, f) => Func.Base(arg, f)
        }
    }

  def implComma: Parser[String] = "," | "\n"
  def string: Parser[Str[P]] =
    optEnclosed("\"") ^^ { s: String => Str(s.trim) }

  def optEnclosed(sep: String): Parser[String] =
    sep ~> s"[^$sep]".r <~ sep | unreserved
}
