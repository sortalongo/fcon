package co.sortalon.fcon

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import AST._

class ParsersSpec extends FlatSpec
    with Matchers
    with PropertyChecks {

  val parser = new Parsers

  "Parsers" should "detect implied strings" in {
    forAll { (s: String) =>
      val reserved = s"[${Parsers.reservedChars}\n\r]".r
      val hasReservedChar = reserved.findFirstIn(s).isDefined
      val parseSucceeded = parser.parseAll(parser.string, s).successful
      whenever(s.length > 0) {
        parseSucceeded should not equal hasReservedChar
      }
    }
  }

  it should "correctly parse a list" in {
    val eg = "[ 1, 2, 3]"
    val ast = parser(eg).get
    ast shouldEqual Lst(1 to 3 map { i => Str(i.toString) } toList)
  }

  it should "correctly parse a dict" in {
    val eg = "{ foo: too, bar: har }"
    val ast = parser(eg).get
    ast shouldEqual Dict(
      Str("foo") -> Str("too") ::
        Str("bar") -> Str("har") :: Nil
    )
  }

  it should "correctly parse this example" in {
    val eg = """
      [ 1, 2, 3] (arg1, arg2: `arg2` `arg1`)
      { foo: too, bar: har } plus
    """
    val ast = parser(eg)
    ast shouldEqual Merged(
      Lst(1 until 3 map { i => Str(i.toString) } toList) ::
        Func(
          Str("arg1") :: Str("arg2") :: Nil,
          Merged(Sym("arg1") :: Sym("arg2") :: Nil)
        ) ::
        Dict(
          Str("foo") -> Str("too") ::
          Str("bar") -> Str("har") :: Nil
        ) ::
        Str("plus") ::
        Nil
    )
  }

}
