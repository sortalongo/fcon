package co.sortalon.fcon

import AST._
import Parsers.{P, Parsed}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary

object Generators {
  val nonemptyString: Gen[String] =
    Gen.nonEmptyContainerOf[Array,Char](arbitrary[Char]).map(_.mkString)

  def foldGen[A, A2 >: A, B](a: A, bs: Seq[B])(f: (A2, B) => Gen[A2]): Gen[A2] = bs match {
    case Seq() => Gen.oneOf(List(a))
    case Seq(b, tail @ _*) => f(a, b).flatMap { a2 => foldGen(a2, tail)(f) }
  }

  object ASTs {
    type ScopeNP = Scope[Node[P]]
    def addScope(scope: ScopeNP) = { n: Node[P] => (n, scope) }
    val dropScope = { (n: Node[P], s: ScopeNP) => n }.tupled

    val atom: Gen[Sym.Atom] = for (str <- nonemptyString) yield Sym.Atom(str)
    val sym: Gen[Sym] = for (strs <- Gen.listOf(nonemptyString)) yield Sym(strs: _*)

    def pickSym(scope: ScopeNP): Gen[Sym] = {
      val here = Gen.oneOf(scope.map.map { case (sym, _) => sym }.toSeq )
      val below = for {
        (pfx, scope) <- Gen.oneOf(scope.children.toSeq)
        sym <- pickSym(scope)
      } yield Sym.Scoped(pfx, sym)

      scope match {
        case _: Scope.Tree[_] =>
          Gen.oneOf(here, below)
        case e: Scope.Embedded[Node[P]] @ unchecked =>
          val above = pickSym(e.parent)
          Gen.oneOf(above, here, below)
      }
    }
    
    val str: Gen[Str[P]] = for (str <- nonemptyString) yield Str(str)
    implicit val arbStr = Arbitrary(str)

    def lst(scope: ScopeNP): Gen[Lst[P]] = for {
      nodes <- Gen.listOf(node(scope))
    } yield Lst(nodes.map(_._1))
    val lst_ = lst(Scope.Empty)
    implicit val arbLst = Arbitrary(lst_)

    def pair(scope: ScopeNP): Gen[(Pair[P], ScopeNP)] = for {
      key <- atom
      (value, scope2) <- node(scope.branch(key))
    } yield (Pair(key, value), scope2.climb(value))


    def dict(scope: ScopeNP): Gen[(Dict[P], ScopeNP)] = Gen.sized { size =>
      val pairsGen =  foldGen((List.empty[Pair[P]], scope), (0 until size)) { (accum, _) =>
        val (pairs, scope2) = accum
        for {
          (pair, scope3) <- pair(scope2)
        } yield (pair :: pairs, scope3)
      }

      for ((pairs, scope4) <- pairsGen)
      yield (Dict(pairs.reverse), scope4)
    }
    val dict_ = dict(Scope.Empty).map(_._1)
    implicit val arbDict = Arbitrary(dict_)

    def func(scope: ScopeNP): Gen[Func[P, P]] = {
      for {
        arg <- atom
        body <- {
          val sub = scope.branch(arg)
          val dnode = node(sub).map(dropScope)
          Gen.oneOf(func(sub), dnode)
        }
      } yield Func.Base(arg, body)
    }
    val func_ = func(Scope.Empty)
    implicit val arbFunc = Arbitrary(func_)

    def merged(scope: ScopeNP): Gen[Merged[P]] = {
      val node_ = node(scope)
      for (nodes <- Gen.listOf(node_)) yield Merged(nodes.map(dropScope))
    }
    val merged_ = merged(Scope.Empty)
    implicit val arbMerged = Arbitrary(merged_)

    def node(scope: ScopeNP): Gen[(Node[P], ScopeNP)] = {
      val add = addScope(scope)
      val scStr = str.map(add)
      val scLst = lst(scope).map(add)
      val scFunc = func(scope).map(add)
      val scMerged = merged(scope).map(add)
      Gen.oneOf(scStr, scLst, dict(scope), scFunc, scMerged)
    }
    val node_ = node(Scope.Empty)
    implicit val arbNode = Arbitrary(node_)
  }

  object Scopes {
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

    val ops: Gen[List[Op]] = Gen.listOf(
      Gen.frequency(1 -> bind, 3 -> branch, 2 -> climb)
    )
    implicit val arbOps = Arbitrary(ops)

    val scope: Gen[Scope[Int]] = ops.map(ops => fold(ops))
    implicit val arbScope = Arbitrary(scope)
  }
}
