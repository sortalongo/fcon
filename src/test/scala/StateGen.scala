package co.sortalon.fcon

import org.scalacheck.Gen
import scalaz.StateT
import scalaz.scalacheck.ScalaCheckBinding.GenMonad

object StateGen {

  type StateGen[State, T] = StateT[Gen, State, T]

  // to use the MonadState methods:
  // val ops = monad[ *state type* ]
  // import ops._
  def monad[S] = StateT.stateTMonadState[S, Gen]

  // to use the MonadTrans methods:
  // val opsT = trans[ *state type* ]
  // import opsT._
  def trans[S] = StateT.StateMonadTrans[S]
}
