package co.sortalon.fcon

import org.scalacheck.{Gen}
import org.scalacheck.Arbitrary.arbitrary

package object generators {
  val nonemptyString: Gen[String] =
    Gen.nonEmptyContainerOf[Array,Char](arbitrary[Char]).map(_.mkString)
}

