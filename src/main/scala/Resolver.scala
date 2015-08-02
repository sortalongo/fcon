package co.sortalon.fcon

import AST._
import Parsers.P
import scala.util.Try

object Resolver {
  case class Resolved(
    scope: Scope = Scope.Empty
  ) extends Stage
  type R = Resolved

  class ResolutionError(s: String) extends Exception(s)
  class OutOfScopeError(s: String) extends Exception(s)

  def apply(ast: Node[P]): Try[Node[R]] = Try(resolve(ast, Scope.Empty))

  private[fcon] def resolve(
    node: Node[P],
    scope: Scope
  ): Node[R] = node match {

    // Strs are the simple base case: they are constants that do not need to be resolved.
    // Thus, we simply pass them back with the same scope
    case s @ Str(_) =>
      s.copy()(Resolved(scope))

    // Syms are the base case which yields a substitution.
    // We perform the substitution by looking up the name in the scope.
    case s: Sym =>
      scope(s).getOrElse {
        throw new OutOfScopeError(s"$s not found in scope $scope")
      }

    // Lists simply pass on their scope to their elements
    case Lst(elems) =>
      Lst(elems.map(resolve(_, scope)))(Resolved(scope))

    case Merged(nodes) =>
      Ops.merge(nodes.map(resolve(_, scope))).get

    // Dicts create new nested scopes, passing those on to their values,
    // and accumulating a larger un-nested scope for following key-value pairs
    case Dict(pairs) =>
      val reversedDict = pairs.foldLeft(Dict(Nil)(Resolved(scope))) {
        case (dict, Pair(k, v)) =>
          val inScope = dict.stage.scope

          val resolvedV = resolve(v, inScope.branch(k))
          val Resolved(resolvedScope) = resolvedV.stage
          val outPair = Pair(k, resolvedV)(Resolved(resolvedScope))

          val outScope = resolvedScope.climb(resolvedV)
          Dict(outPair :: dict.pairs)(Resolved(outScope))
      }
      Dict(reversedDict.pairs.reverse)(reversedDict.stage)

    case f: Func.Base[P, P]@unchecked =>
      val r = Resolved(scope)
      Func.Base[R, P](
        f.arg.copy()(r),
        f.body
      )(r)

    case _ =>
      throw new ResolutionError(s"Resolving invalid node $node")
  }
}
