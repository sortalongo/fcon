package co.sortalon.fcon

import org.scalacheck.Gen
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.PropertyChecks
import scala.util.Success

class OpsSpec extends FunSuite
    with Matchers
    with PropertyChecks {
  import Parsers.P
  import Resolver.R
  import AST._
  import generators.ASTs._

  test("two merged lists have n * m elements") {
    forAll { (l1: Lst[P], l2: Lst[P]) =>
      val toMerge = List(l1, l2).map {
        l => Resolver(l).get
      }
      val Success(Lst(result)) = Ops.merge(toMerge)
      result should have length (l1.elems.length * l2.elems.length)
    }
  }

  // these need a traversal combinator
  // test("a string on the right ends up at the end of every element")
  // test("a string on the left ends up at the beginning of every element")

  test("a list merged with a non-list has n elements") {
    forAll(lst, Gen.oneOf(str, dict)) { (l: Lst[P], n: Node[P]) =>
      val toMerge = List(l, n).map {
        x => Resolver(x).get
      }
      val Success(Lst(result)) = Ops.merge(toMerge)
      result should have length (l.elems.length)
    }
  }
  // test("a dict merged with a dict has union(left, right) elements")
  // test("a dict merged with a non-dict has n elements")
  // test("an applied function has its arg bound in scope")
  // test("a collection on the left determines the output type of a merge")
}
