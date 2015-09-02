package co.sortalon.fcon

import AST._
import Parsers.P
import Resolver.{R, Resolved}
import scala.annotation.tailrec
import scala.collection.breakOut
import scala.util.{Try, Success, Failure}

object Ops {
  class MergeError(s: String) extends Exception(s)

  @tailrec
  def merge(nodes: List[Node[R]]): Try[Node[R]] = nodes match {
    // FEATURE: scopes into fully-resolved scopes
    // we must access the scope of the parent of the merge, merging
    // it with the results of the merge.
    case Nil => Failure(new MergeError(s"Unable to merge empty list"))
    case elem :: Nil => Success(elem)
    case fst :: snd :: rest =>
      val combined = Try(merge(fst, snd))
      combined match {
        case Success(head) =>  merge(head :: rest)
        case failure => failure
      }
  }
  // At this point, all symbols not inside functions have already been resolved
  private def merge(left: Node[R], right: Node[R]): Node[R] = (left, right) match {
    case (_, f: Func[R, P]@unchecked) => Func.Deferred[R, P](left, f)
    case (s: Str[_], _) => mergeString(s, right)
    case (l: Lst[_], _) => mergeList(l, right, merge(_, right))
    case (d: Dict[_], _) => mergeDict(d, right, merge(_, right))
    case (f: Func[R, P]@unchecked, _) => applyFunc(f, right)
    case _ =>
      throw new MergeError(s"Encountered unmergeable nodes $left and $right")
  }

  private def mergeString(s: Str[R], right: Node[R]): Node[R] = right match {
    case s2: Str[_] => Str(s.s + s2.s)(s.stage)
    case l: Lst[_] => mergeList(l, s, merge(s, _))
    case d: Dict[_] => mergeDict(d, s, merge(s, _))
    case _ =>
      throw new MergeError(s"Cannot merge $s with $right")
  }

  private def mergeList(l: Lst[R], right: Node[R], mergeFn: Node[R] => Node[R]) = {
    implicit val stage = l.stage

    right match {
      case Lst(elems) =>
        val product = for (
          i <- l.elems;
          j <- elems
        ) yield merge(i, j)

        Lst(product)

      case r =>
        Lst(l.elems.map(mergeFn))
    }
  }

  private def mergeDict(l: Dict[R], right: Node[R], mergeFn: Node[R] => Node[R]) = {
    implicit val stage = l.stage

    right match {
      case Dict(pairs: List[Pair[R]]@unchecked) =>
        val map: Map[Sym.Atom, Node[R]] = l.pairs.map {
          case Pair(k, v) => (k, v)
        }(breakOut)

        val newMap = pairs.foldLeft(map) {
          case (foldMap: Map[Sym.Atom, Node[R]]@unchecked, Pair(k, vr)) =>
            foldMap.get(k) match {
              case Some(vl) =>
                foldMap + (k -> merge(vl, vr))
              case None =>
                foldMap + (k -> vr)
            }
        }
        Dict(newMap.map((Pair(_: Sym.Atom, _: Node[R])).tupled).toList)

      case _ =>
        val newPairs = l.pairs.map {
          case Pair(k, v) => Pair(k, mergeFn(v))
        }
        Dict(newPairs)
    }
  }

  private def applyFunc(f: Func[R, P], right: Node[R]): Node[R] = f match {
    case Func.Deferred(left, subFunc) =>
      merge(left, applyFunc(subFunc, right))
    case Func.Base(arg, body) =>
      // bind the function's substitution inside the function's scope
      val newScope = f.stage.scope.bind(arg, right)
      // resolve the function body with the new binding
      Resolver.resolve(body, newScope)
  }
}
