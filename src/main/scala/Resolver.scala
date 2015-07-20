package co.sortalon.fcon

import AST._
import Parsers.Parsed

object Resolver {
  case class Resolved(
    parent: Option[Node[Resolved]] = None,
    scope: Option[Map[Str[Parsed.type]]] = None
  ) extends Stage

  def apply(ast: Node[Parsed.type]): Node[Resolved] = {
  }

  private def recurse(node: Node[Parsed.type], parent: Node[Parsed.type]): Node[Resolved] = {
  }
}
