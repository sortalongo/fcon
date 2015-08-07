package co.sortalon.fcon

import AST._
import Parsers.{P, Parsed}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary

object Generators {
  val nonemptyString: Gen[String] =
    Gen.nonEmptyContainerOf[Array,Char](arbitrary[Char]).map(_.mkString)

  object ASTs {

    val atom: Gen[Sym.Atom] = for (str <- nonemptyString) yield Sym.Atom(str)
    val sym: Gen[Sym] = for (strs <- Gen.listOf(nonemptyString)) yield Sym(strs: _*)
    
    val str: Gen[Str[P]] = for (str <- nonemptyString) yield Str(str)
    implicit val arbStr = Arbitrary(str)

    val lst: Gen[Lst[P]] = for {
      nodes <- Gen.listOf(node)
    } yield Lst(nodes)
    implicit val arbLst = Arbitrary(lst)

    val dict: Gen[Dict[P]] = {
      val pair = for {
        key <- atom
        value <- node
      } yield Pair(key, value)

      for (pairs <- Gen.listOf(pair)) yield Dict(pairs)
    }
    implicit val arbDict = Arbitrary(dict)

    val func: Gen[Func[P, P]] = {
      for {
        arg <- str
        body <- Gen.oneOf(func, node)
      } yield Func.Base(arg, body)
    }
    implicit val arbFunc = Arbitrary(func)

    val merged: Gen[Merged[P]] =
      for (nodes <- Gen.listOf(node)) yield Merged(nodes)
    implicit val arbMerged = Arbitrary(merged)

    val node: Gen[Node[P]] = Gen.oneOf(str, lst, dict, func, merged)
    implicit val arbNode = Arbitrary(node)
  }

  object Scopes {
    sealed trait Op
    case class Branch(sym: Sym.Atom) extends Op
    case class Bind(sym: Sym.Atom, i: Int) extends Op
    case class Climb(i: Int) extends Op

    def fold(ops: List[Op], scope: Scope[Option[Int]] = Scope.Empty): Scope[Option[Int]] = ops match {
      case Nil => scope
      case Branch(sym) :: tail => fold(tail, scope.branch(sym))
      case Climb(i) :: tail => fold(tail, scope.climb(Some(i)))
      case Bind(sym, i) :: tail => fold(tail, scope.bind(sym, Some(i)))
    }

    val branch = ASTs.atom.map(Branch(_))
    val bind = for {
      sym <- ASTs.atom
      i <- arbitrary[Int]
    } yield Bind(sym, i)
    val climb = arbitrary[Int].map(Climb(_))

    val ops: Gen[List[Op]] = Gen.listOf(
      Gen.frequency(1 -> bind, 3 -> branch, 2 -> climb)
    )
    implicit val arbOps = Arbitrary(ops)

    val scope: Gen[Scope[Option[Int]]] = ops.map(ops => fold(ops))
    implicit val arbScope = Arbitrary(scope)
  }
}
