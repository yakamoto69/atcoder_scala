import org.scalacheck.{Arbitrary, Gen}

package object testlang {

  import org.scalacheck.Shrink

  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  def genNum(max: Int): Gen[Int] = Gen.chooseNum(1, max)
  def genBool = Arbitrary.arbBool.arbitrary
}
