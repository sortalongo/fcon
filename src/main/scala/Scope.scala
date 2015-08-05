package co.sortalon.fcon

import AST._
import Resolver.R

sealed trait Scope {
  def apply(sym: Sym): Option[Node[R]]
  def bind(name: Sym.Atom, node: Node[R]): Scope

  def branch(sym: Sym.Atom): Scope
  def climb(node: Node[R]): Scope

  protected def map: Map[Sym.Atom, Node[R]]
  protected def children: Map[Sym.Atom, Scope]

}

object Scope {

  def apply(pairs: (Sym.Atom, Node[R])*): Scope = Tree(Map(pairs: _*), Map.empty)

  val Empty: Scope = Tree(Map.empty, Map.empty)

  private[fcon] case class Tree(
    map: Map[Sym.Atom, Node[R]],
    children: Map[Sym.Atom, Scope]
  ) extends Scope {
    def apply(sym: Sym) =  sym match {
      case s: Sym.Atom => map.get(s)
      case Sym.Scoped(head, tail) => children.get(head).flatMap {
        child => child(tail)
      }
    }

    def bind(
      name: Sym.Atom,
      node: Node[R]
    ): Scope = Tree(map + (name -> node), children)

    def branch(sym: Sym.Atom) = Embedded(sym, this)
    def climb(node: Node[R]) = this
  }

  private[fcon] case class Embedded(
    symbol: Sym.Atom,
    parent: Scope,
    map: Map[Sym.Atom, Node[R]] = Map.empty,
    children: Map[Sym.Atom, Scope] = Map.empty
  ) extends Scope {
    def apply(sym: Sym) = toTree(sym).orElse(parent(sym))

    def bind(
      name: Sym.Atom,
      node: Node[R]
    ): Scope = Embedded(symbol, parent, map + (name -> node), children)

    def branch(sym: Sym.Atom) = Embedded(sym, this)
    def climb(node: Node[R]) = parent match {
      case Tree(m2, ch2) =>
        Tree(m2, ch2 + (symbol -> toTree))
      case Embedded(sym2, p2, m2, ch2) =>
        Embedded(sym2, p2, m2, ch2 + (symbol -> toTree))
    }
    private def toTree = Tree(map, children)
  }
}
