
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class SortSpec extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {
  "radioxSort" should "sort positive Integers" in {
    forAll(Gen.listOf(Gen.posNum[Int])) { base =>
      val as = base.toArray
      collection.radixSort(as) should be(base.sorted.toArray)
    }
  }

  "radioxSort" should "sort positive Longs" in {
    forAll(Gen.listOf(Gen.posNum[Long])) { base =>
      val as = base.toArray
      collection.radixSort(as) should be(base.sorted.toArray)
    }
  }
}
