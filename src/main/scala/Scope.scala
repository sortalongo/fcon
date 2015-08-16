package co.sortalon.fcon

import AST._
import Resolver.R

sealed trait Scope[T] {
  def apply(sym: Sym): Option[T]
  def bind(name: Sym.Atom, node: T): Scope[T]

  def branch(sym: Sym.Atom): Scope[T]
  def climb(node: T): Scope[T]

  def map: Map[Sym.Atom, T]
  def children: Map[Sym.Atom, Scope.Tree[T]]

  def isEmpty: Boolean
}

object Scope {

  def apply[T](pairs: (Sym.Atom, T)*): Scope[T] = Tree[T](Map(pairs: _*), Map.empty)

  def Empty[T]: Scope[T] = Tree[T](Map.empty, Map.empty)

  private[fcon] case class Tree[T](
    map: Map[Sym.Atom, T],
    children: Map[Sym.Atom, Scope.Tree[T]]
  ) extends Scope[T] {
    def apply(sym: Sym) =  sym match {
      case s: Sym.Atom => map.get(s)
      case Sym.Scoped(head, tail) => children.get(head).flatMap {
        child => child(tail)
      }
    }

    def bind(
      name: Sym.Atom,
      node: T
    ): Scope[T] = Tree[T](map + (name -> node), children)

    def branch(sym: Sym.Atom) = Embedded[T](sym, this)
    def climb(node: T) = this

    def isEmpty = map.isEmpty && children.isEmpty
  }

  private[fcon] case class Embedded[T](
    symbol: Sym.Atom,
    parent: Scope[T],
    map: Map[Sym.Atom, T] = Map.empty[Sym.Atom, T],
    children: Map[Sym.Atom, Tree[T]] = Map.empty[Sym.Atom, Tree[T]]
  ) extends Scope[T] {
    def apply(sym: Sym) = toTree(sym).orElse(parent(sym))

    def bind(
      name: Sym.Atom,
      node: T
    ): Scope[T] = Embedded[T](symbol, parent, map + (name -> node), children)

    def branch(sym: Sym.Atom) = Embedded[T](sym, this)
    def climb(node: T) = {
      val map2 = parent.map + (symbol -> node)
      val children2 = {
        val tree = toTree
        if (tree.isEmpty) parent.children
        else parent.children + (symbol -> tree)
      }

      parent match {
        case Tree(m2, ch2) =>
          Tree[T](map2, children2)
        case Embedded(sym2, p2, _, _) =>
          Embedded[T](sym2, p2, map2, children2)
      }
    }
    private def toTree = Tree[T](map, children)

    def isEmpty = map.isEmpty && children.isEmpty && parent.isEmpty
  }
}
