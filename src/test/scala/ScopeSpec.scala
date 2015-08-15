package co.sortalon.fcon

import AST._
import generators.Scopes
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.PropertyChecks

class ScopeSpec extends FunSuite
    with Matchers
    with PropertyChecks {

  import Scopes._
  import ScopeSpec._

  test("scopes can find all unshadowed nodes") {
    forAll { (scope: Scope[Int]) =>
      unshadowedPairs(scope).foreach {
        case (sym, node) =>
          scope(sym) shouldEqual Some(node)
      }
    }
  }
}

object ScopeSpec {

  def members(
    scope: Scope[Int],
    prefix: Seq[String] = Vector.empty
  ): Map[Sym, Int] = {
    val newMems = scope.map.map {
      case (sym, i) => (Sym.sub(sym, prefix), i)
    }
    scope.children.foldLeft(newMems) {
      case (accum, (sym, sub)) =>
        accum ++ members(sub, prefix :+ sym.s)
    }
  }

  def unshadowedPairs(
    scope: Scope[Int],
    embedPfx: Seq[String] = Vector.empty
  ): Map[Sym, Int] = scope match {
    case _: Scope.Tree[Int] @ unchecked =>
      members(scope)
    case embed: Scope.Embedded[Int] @ unchecked =>
      // maps resolve duplicate keys with last-write-wins
      // we take advantage of this behavior to only return unshadowed symbols
      // by appending higher-priority scopes to lower-priority ones
      unshadowedPairs(embed.parent, embedPfx :+ embed.symbol.s) ++ members(scope)
  }
}
