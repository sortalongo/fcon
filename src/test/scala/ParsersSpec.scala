package co.sortalon.fcon

import Parsers.Parsed
import AST._
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.PropertyChecks
import scala.language.postfixOps

class ParsersSpec extends FunSuite
    with Matchers
    with PropertyChecks {

  val parser = new Parsers

  //  test("arbitrary JSON leads to a successful parse")

  test("nonempty strings with no reserved chars are implied strings") {
    forAll { (s: String) =>
      val reserved = s"[${Parsers.reservedChars}${Parsers.quoteChars}\n\r]".r
      val hasReservedChar = reserved.findFirstIn(s).isDefined
      val parseSucceeded = parser.parseAll(parser.string, s).successful
      whenever(s.length > 0) {
        if (parseSucceeded == hasReservedChar) {
          s.count(_ == '"') shouldEqual 2
          s should (startWith ("\"") and endWith ("\""))
        }
        else parseSucceeded should not equal hasReservedChar
      }
    }
  }

  test("parse a list") {
    val eg = " [ 1, 2, 3 ] "
    val ast = parser(eg).get
    ast shouldEqual Lst(1 to 3 map { i => Str(i.toString) } toList)
  }

  test("parse a dict") {
    val eg = " { foo: too, bar: har } "
    val ast = parser(eg).get
    ast shouldEqual Dict(
      Pair(Sym.Atom("foo"), Str("too")) ::
        Pair(Sym.Atom("bar"), Str("har")) :: Nil
    )
  }

  test("parse a function") {
    val eg = "(x,y, z: `x` `y` `z`) "
    val ast = parser(eg).get
    ast shouldEqual Func.Base(Str("x"),
      Func.Base(Str("y"),
        Func.Base(Str("z"),
          Merged(List(Sym("x"), Sym("y"), Sym("z")))
        )))
  }

  test("obey order of operations with parens") {
    val eg = """
      [ 1, 2, 3] (arg1, arg2: `arg2` `arg1`)
      ({ foo: too, bar: har } plus)
    """
    val ast = parser(eg).get
    ast shouldEqual Merged(
      Lst(1 to 3 map { i => Str(i.toString) } toList) ::
        Func.Base(Str("arg1"),
          Func.Base(Str("arg2"),
            Merged(Sym("arg2") :: Sym("arg1") :: Nil)
        )) ::
        Merged(
          Dict(
            Pair(Sym.Atom("foo"), Str("too")) ::
            Pair(Sym.Atom("bar"), Str("har")) :: Nil
          ) :: Str("plus") :: Nil
        ) :: Nil
    )
  }

}
