package co.sortalon.fcon

import AST._
import Parsers.{P, Parsed}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import scalaz.State

object Generators {
  val nonemptyString: Gen[String] =
    Gen.nonEmptyContainerOf[Array,Char](arbitrary[Char]).map(_.mkString)

  def foldGen[A, A2 >: A, B](a: A, bs: Seq[B])(f: (A2, B) => Gen[A2]): Gen[A2] = bs match {
    case Seq() => Gen.oneOf(List(a))
    case Seq(b, tail @ _*) => f(a, b).flatMap { a2 => foldGen(a2, tail)(f) }
  }

  object ASTs {
    import StateGen.{StateGen, monad}

    type ScopeNP = Scope[Node[P]]
    type State = (Int, ScopeNP)

    val ops = StateGen.monad[State]
    import ops.{get, modify, replicateM}
    import ops.{liftM}

    val seedSize = 50
    val maxLength = 10

    val atom: Gen[Sym.Atom] = for (str <- nonemptyString) yield Sym.Atom(str)
    val sym: Gen[Sym] = for (strs <- Gen.listOf(nonemptyString)) yield Sym(strs: _*)

    val pickSym = State[(Int, ScopeNP), Gen[Sym]] ({ (size, scope) =>
      val here = Gen.oneOf(scope.map.map { case (sym, _) => sym }.toSeq )
      val below = for {
        (pfx, scope) <- Gen.oneOf(scope.children.toSeq)
        sym <- pickSym(scope)
      } yield Sym.Scoped(pfx, sym)

      val gen = scope match {
        case _: Scope.Tree[_] =>
          Gen.oneOf(here, below)
        case e: Scope.Embedded[Node[P]] @ unchecked =>
          val above = pickSym(e.parent)
          Gen.oneOf(above, here, below)
      }
      ((size - 1, scope), gen)
    }.tupled)
    
    val str = State[(Int, ScopeNP), Gen[Str[P]]] ({ (size, scope) =>
      val gen = for (str <- nonemptyString) yield Str(str)
      ((size - 1, scope), gen)
    }.tupled)
    implicit val arbStr = Arbitrary(str.eval((1, Scope.Empty)))

    val lst: StateGen[State, Lst[P]] = for {
      (size, _) <- get
      count <- liftM(Gen.choose(0, maxLength min size))
      nodes <- replicateM(count, node)
    } yield Lst(nodes)
    val lst_ = lst.eval((seedSize, Scope.Empty))
    implicit val arbLst = Arbitrary(lst_)

    def pair(scope: ScopeNP): Gen[(Pair[P], ScopeNP)] = for {
      key <- atom
      (value, scope2) <- node(scope.branch(key))
    } yield (Pair(key, value), scope2.climb(value))


    def dict(scope: ScopeNP): Gen[(Dict[P], ScopeNP)] = Gen.choose(1, maxLength).flatMap { size =>
      val pairsGen = foldGen((List.empty[Pair[P]], scope), (0 until size)) { (accum, _) =>
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

    def merged(scope: ScopeNP): Gen[Merged[P]] = Gen.choose(1, maxLength).flatMap { size =>
      val node_ = node(scope)
      for (nodes <- Gen.listOfN(size, node_)) yield Merged(nodes.map(dropScope))
    }
    val merged_ = merged(Scope.Empty)
    implicit val arbMerged = Arbitrary(merged_)

    def node(scope: ScopeNP): Gen[(Node[P], ScopeNP)] = {
      val add = addScope(scope)
      // pack generators into functions to avoid infinite recursion
      val lazyArgs = List(
        () => str.map(add),
        () => lst(scope).map(add),
        () => dict(scope),
        () => func(scope).map(add),
        () => merged(scope).map(add)
      )
      val anyGen = for {
        argFn <- Gen.oneOf(lazyArgs)
        arg <- argFn()
      } yield arg
      Gen.frequency(
        1 -> str.map(add),
        1 -> anyGen
      )
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
