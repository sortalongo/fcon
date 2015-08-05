package co.sortalon.fcon

import Parsers.P
import Resolver.{R, Resolved}
import AST._
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.PropertyChecks
import scala.util.Success

class OpsSpec extends FunSuite
    with Matchers
    with PropertyChecks {

  test("two merged lists have n * m elements") {
    forAll { (l1: List[String], l2: List[String]) =>
      implicit val res = Resolved()
      val lst1 = Lst(l1.map(Str(_)))
      val lst2 = Lst(l2.map(Str(_)))
      val Success(Lst(result)) = Ops.merge(List(lst1, lst2))
      result should have length (l1.length * l2.length)
    }
  }

  // these need a traversal combinator
  // test("a string on the left ends up at the end of every element")
  // test("a string on the right ends up at the beginning of every element")

  // test("a list merged with a non-list has n elements")
  // test("a dict merged with a dict has union(left, right) elements")
  // test("a dict merged with a non-dict has n elements")
  // test("an applied function has its arg bound in scope")
  // test("a collection on the left determines the output type of a merge")
}
