package co.sortalon.fcon

import AST._
import Parsers.{P, Parsed}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary

object Generators {
  val nonemptyString: Gen[String] =
    Gen.nonEmptyContainerOf[Array,Char](arbitrary[Char]).map(_.mkString)

  object AST {

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
}
