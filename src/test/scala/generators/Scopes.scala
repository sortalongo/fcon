package co.sortalon.fcon.generators

import co.sortalon.fcon._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen.const
import org.scalacheck.Arbitrary.arbitrary
import scalaz.State

object Scopes {
  import AST.Sym

  // generators and arbitrary instances
  lazy val ops: Gen[List[Op]] = Gen.listOf(
    Gen.frequency(1 -> bind, 3 -> branch, 2 -> climb)
  )
  lazy implicit val arbOps = Arbitrary(ops)

  lazy val scope: Gen[Scope[Int]] = ops.map(ops => fold(ops))
  lazy implicit val arbScope = Arbitrary(scope)

  // support objects
  sealed trait Op
  case class Branch(sym: Sym.Atom) extends Op
  case class Bind(sym: Sym.Atom, i: Int) extends Op
  case class Climb(i: Int) extends Op

  def fold(ops: List[Op], scope: Scope[Int] = Scope.Empty): Scope[Int] = ops match {
    case Nil => scope
    case Branch(sym) :: tail => fold(tail, scope.branch(sym))
    case Climb(i) :: tail => fold(tail, scope.climb(i))
    case Bind(sym, i) :: tail => fold(tail, scope.bind(sym, i))
  }

  val branch = ASTs.atom.map(Branch(_))
  val bind = for {
    sym <- ASTs.atom
    i <- arbitrary[Int]
  } yield Bind(sym, i)
  val climb = arbitrary[Int].map(Climb(_))

}

