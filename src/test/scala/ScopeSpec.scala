package co.sortalon.fcon

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.PropertyChecks

class ScopeSpec extends FunSuite
    with Matchers
    with PropertyChecks {

  import Generators.Scopes._
  import AST._

  test("root lookups of embedded scopes are equiv to tree lookup") {
    forAll { (scope: Scope[Option[Int]]) =>
      val tree = root(scope)
      foreach(tree) { (rooted, iOpt) =>
        if (iOpt.isDefined)
          scope(rooted).flatten shouldEqual iOpt
      }
    }
  }

  def root(s: Scope[Option[Int]]): Scope.Tree[Option[Int]] = {
    // we mark climbed nodes as None to identify synthesized data
    val climb = s.climb(None)
    if (climb eq s) s.asInstanceOf[Scope.Tree[Option[Int]]]
    else root(climb)
  }

  def foreach(
    tree: Scope.Tree[Option[Int]]
  )(check: (Sym, Option[Int]) => Unit
  ) = {

    def recurse(
      tree: Scope.Tree[Option[Int]],
      syms: Seq[String]
    ): Unit = {
      tree.map.foreach {
        case (Sym.Atom(sym), iOpt) => check(Sym.rooted(syms :+ sym: _*), iOpt)
      }
      tree.children.foreach {
        case (Sym.Atom(sym), subtree) => recurse(subtree, syms :+ sym)
      }
    }

    recurse(tree, Seq.empty)
  }
}
