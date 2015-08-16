package co.sortalon.fcon.generators

import co.sortalon.fcon.{AST, Parsers, Scope, StateGen}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen.const
import org.scalacheck.Arbitrary.arbitrary
import scalaz.State
import scala.math.BigInt

object ASTs {
  import AST._
  import Parsers.{Parsed, P}

  // Generators and Arbitrary instances
  val identifier = Gen.resize(10, Gen.identifier)

  lazy val atom: Gen[Sym.Atom] = for (str <- identifier) yield Sym.Atom(str)
  lazy val sym: Gen[Sym] = for (strs <- Gen.listOf(identifier)) yield Sym(strs: _*)

  lazy val str: Gen[Str[P]] = evalZero(strSG)
  lazy implicit val arbStr = Arbitrary(str)

  lazy val lst: Gen[Lst[P]] = evalZero(lstSG)
  lazy implicit val arbLst = Arbitrary(lst)

  lazy val dict: Gen[Dict[P]] = evalZero(dictSG)
  lazy implicit val arbDict = Arbitrary(dict)

  lazy val func: Gen[Func[P, P]] = evalZero(funcSG)
  lazy implicit val arbFunc = Arbitrary(func)

  lazy val merged: Gen[Merged[P]] = evalZero(mergedSG)
  lazy implicit val arbMerged = Arbitrary(merged)

  lazy val node: Gen[Node[P]] = evalZero(nodeSG)
  lazy implicit val arbNode = Arbitrary(node)

  // StateGen convenience imports and methods

  type ScopeNP = Scope[Node[P]]
  type State = (Int, ScopeNP)

  import StateGen.{StateGen, monad, trans}
  import scalaz.Functor
  import scalaz.scalacheck.ScalaCheckBinding.GenMonad
  import scalaz.std.list._

  val ops = monad[State]
  import ops.{put, get, modify, sequence, replicateM}
  val opsT = trans[State]
  import opsT.{liftM}

  val decrement = for {
    _ <- modify( { (i: Int, s: ScopeNP) =>
      print(s"$i ")
      (i - 1, s)
    }.tupled )
    s <- get
  } yield s

  def branch(k: Sym.Atom) =
    modify( { (i: Int, s: ScopeNP) => (i, s.branch(k)) }.tupled )
  def climb(v: Node[P]) =
    modify( { (i: Int, s: ScopeNP) => (i, s.climb(v)) }.tupled )

  /* A composition of a positive integer `num` is a sequence of positive integers
   * summing up to `num`. A composition can be uniquely constructed from 2^(num - 1)
   * bits as follows:
   *   composition(num) = Seq(1 _ 1 _ ... _ 1)
   *   where `num` - 1 blanks can be either a '+' or ','
   *   and a '+' is denoted by a 1 bit, and a ',' is a 0 bit
   */
  def composition(num: Int, d: BigInt, comp: List[Int] = Nil): List[Int] =
    if (num < 0) Nil
    else if (num == 0) comp
    else
      composition(
        num - 1,
        d >> 1,
        if (d.testBit(0))
          comp.headOption.map(_ + 1 :: comp.tail).getOrElse(List(1))
        else 1 :: comp
      )

  /* Given a state and a state generator, this method creates a composition from the
   * `size`, then maps the state generator over the parts of the composition, forwarding
   * `scope` to each generator, and finally sequencing the generators into a single one.
   */
  def sequenceComposition[T](
    state: State,
    sg: StateGen[State, T]
  ): StateGen[State, List[T]] = {
    val (size, scope) = state
    for {
      // generates a random bigint and uses it to create a composition
      bytes <- liftM(Gen.containerOfN[Array, Byte](size / 8 + 1, arbitrary[Byte]))
      comp = composition(size, BigInt(bytes).abs)
      // maps state generators over composition, then sequences them
      sgs = comp.map { part =>
        modify( { (i: Int, s: ScopeNP) => (part, s) }.tupled )
        .flatMap(_ => sg)
      }
      nodes <- sequence(sgs)
    } yield nodes
  }

  def cast[T](sg: StateGen[State, T]): StateGen[State, Node[P]] =
    sg.asInstanceOf[StateGen[State, Node[P]]]

  def evalZero[T](sg: StateGen[State, T]): Gen[T] =
    Gen.sized { size => sg.eval((size, Scope.Empty)) }

  // Generator implementations
  def arbSym(scope: ScopeNP): Gen[Sym] = {
    val here = Gen.oneOf(scope.map.keys.toSeq)
    val below = for {
      childPair <- Gen.oneOf(scope.children.toSeq)
      (pfx, subscope) = childPair
      belowSym <- arbSym(subscope)
    } yield Sym.Scoped(pfx, belowSym)
    val above = scope match {
      case _: Scope.Tree[_] => Gen.fail
      case e: Scope.Embedded[Node[P]] @ unchecked =>
        arbSym(e.parent)
    }
    Gen.wrap {
      Gen.oneOf((here.sample ++ below.sample ++ above.sample).toSeq)
    }
  }
  val pickSym: StateGen[State, Sym] = for {
    st <- decrement
    (size, scope) = st
    sym <- liftM(arbSym(scope))
  } yield sym

  val strSG: StateGen[State, Str[P]] = for {
    _ <- decrement
    s <- liftM(identifier)
  } yield Str(s)

  val lstSG: StateGen[State, Lst[P]] = for {
    state <- decrement
    nodes <- sequenceComposition(state, nodeSG)
  } yield Lst(nodes)

  val pairSG: StateGen[State, Pair[P]] = for {
    key <- liftM(atom)
    _ <- branch(key)
    value <- nodeSG
    _ <- climb(value)
  } yield Pair(key, value)

  val dictSG: StateGen[State, Dict[P]] = for {
    state <- decrement
    pairs <- sequenceComposition(state, pairSG)
  } yield Dict(pairs)

  val funcSG: StateGen[State, Func[P, P]] = for {
    st <- get
    (_, scope) = st

    arg <- liftM(atom)
    _ <- branch(arg)

    body <- nodeSG

    // drop the branched scope so it doesn't leak out of the function
    _ <- modify( { (size: Int, _: ScopeNP) => (size, scope) }.tupled )
  } yield Func.Base(arg, body)

  val mergedSG: StateGen[State, Merged[P]] = for {
    state <- get
    nodes <- sequenceComposition(state, nodeSG)
  } yield Merged(nodes)

  val nodeSG: StateGen[State, Node[P]] = for {
    st <- get
    (size, scope) = st
    sym = if (scope.isEmpty) Nil else List(cast(pickSym))
    terminals = cast(strSG) :: sym
    nodes = List(
      cast(lstSG),
      cast(dictSG),
      cast(funcSG),
      cast(mergedSG)
    )
    generators: List[StateGen[State, Node[P]]] = if (size > 1) nodes else terminals
    choice <- liftM(Gen.oneOf(generators))
    node <- choice
  } yield node
}

