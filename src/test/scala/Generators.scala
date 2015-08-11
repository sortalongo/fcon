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
    import StateGen.{StateGen, monad, trans}

    type ScopeNP = Scope[Node[P]]
    type State = (Int, ScopeNP)

    val maxLength = 10

    // Generators and Arbitrary instances

    val atom: Gen[Sym.Atom] = for (str <- nonemptyString) yield Sym.Atom(str)
    val sym: Gen[Sym] = for (strs <- Gen.listOf(nonemptyString)) yield Sym(strs: _*)

    val str: Gen[Str[P]] = evalZero(strSG)
    implicit val arbStr = Arbitrary(str)
    
    val lst: Gen[Lst[P]] = evalZero(lstSG)
    implicit val arbLst = Arbitrary(lst)

    val dict: Gen[Dict[P]] = evalZero(dictSG)
    implicit val arbDict = Arbitrary(dict)

    val func: Gen[Func[P, P]] = evalZero(funcSG)
    implicit val arbFunc = Arbitrary(func)

    val merged: Gen[Merged[P]] = evalZero(mergedSG)
    implicit val arbMerged = Arbitrary(merged)

    val node: Gen[Node[P]] = evalZero(nodeSG)
    implicit val arbNode = Arbitrary(node)

    // StateGen convenience imports and methods
    import scalaz.scalacheck.ScalaCheckBinding.GenMonad
    val ops = monad[State]
    import ops.{get, modify, replicateM}
    val opsT = trans[State]
    import opsT.{liftM}

    val decrement = modify( { (i: Int, s: ScopeNP) => (i - 1, s) }.tupled )
    def branch(k: Sym.Atom) =
      modify( { (i: Int, s: ScopeNP) => (i, s.branch(k)) }.tupled )
    def climb(v: Node[P]) =
      modify( { (i: Int, s: ScopeNP) => (i, s.climb(v)) }.tupled )

    def cast[T](sg: StateGen[State, T]): StateGen[State, Node[P]] =
      sg.asInstanceOf[StateGen[State, Node[P]]]

    def evalZero[T](sg: StateGen[State, T]): Gen[T] =
      Gen.sized { size => sg.eval((size, Scope.Empty)) }

    // Generator implementations

    val pickSym: StateGen[State, Sym] = for {
      _ <- decrement
      (size, scope) <- get
      here = Gen.oneOf(scope.map.keys.toSeq)
      below = for {
        (pfx, scope) <- Gen.oneOf(scope.children.toSeq)
        sym <- pickSym
      } yield Sym.Scoped(pfx, sym)

      choice = scope match {
        case _: Scope.Tree[_] =>
          Gen.oneOf(here, below)
        case e: Scope.Embedded[Node[P]] @ unchecked =>
          val above = put((size, e.parent)).flatMap(_ => pickSym)
          Gen.oneOf(above, here, below)
      }
      sym <- liftM(choice)
    } yield sym

    val strSG: StateGen[State, Str[P]] = for {
      _ <- decrement
      s <- liftM(nonEmptyString)
    } yield s

    val lstSG: StateGen[State, Lst[P]] = for {
      _ <- decrement
      (size, _) <- get
      count <- liftM(Gen.choose(0, maxLength min size))
      nodes <- replicateM(count, nodeSG)
    } yield Lst(nodes)

    val pairSG: StateGen[State, Pair[P]] = for {
      _ <- decrement
      key <- liftM(atom)
      _ <- branch(key)
      value <- nodeSG
      _ <- climb(value)
    } yield Pair(key, value)

    val dictSG: StateGen[State, Dict[P]] = for {
      _ <- decrement
      (size, _) <- get
      n <- liftM(Gen.choose(0, maxLength min size))
      pairs <- replicateM(n, pairSG)
    } yield Dict(pairs)

    val funcSG: StateGen[State, Func[P, P]] = for {
      _ <- decrement
      (_, scope) <- get

      arg <- liftM(atom)
      _ <- branch(arg)

      choice <- liftM(Gen.oneOf(cast(funcSG), nodeSG))
      body <- choice

      // drop the branched scope so it doesn't leak out of the function
      _ <- modify( { (size, _) => (size, scope) }.tupled )
    } yield Func(arg, body)

    val mergedSG: StateGen[State, Merged[P]] = for {
      (size, _) <- get
      n <- liftM(Gen.choose(0, maxLength min size))
      nodes <- replicateM(n, nodeSG)
    } yield Merged(nodes)

    val nodeSG: StateGen[State, Node[P]] = for {
      choice <- liftM(Gen.frequency(
        3 -> cast(strSG),
        1 -> cast(pickSym),
        1 -> cast(lstSG),
        1 -> cast(dictSG),
        1 -> cast(funcSG),
        1 -> cast(mergedSG)
      ))
      node <- choice
    } yield node

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
