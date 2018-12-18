import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen
import string._

class KmpSpec extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {
  "kmp" should "find fist position of word in text" in {
    def genWord = for {
      n <- Gen.chooseNum(1, 5)
      word <- Gen.listOfN(n, Gen.alphaLowerChar)
    } yield word.mkString

    def genHave(word: String) = {
      for {
        prefix <- Gen.alphaStr
        suffix <- Gen.alphaStr
      } yield prefix + word + suffix
    }

    def genArg = {
      for {
        w <- genWord
        randoms <- Gen.listOf(Gen.alphaLowerStr)
        haves <- Gen.listOf(genHave(w))
      } yield (w, randoms ::: haves)
    }

    forAll(genArg) { case (word, texts) =>
      val kmp = new KMP(word)
      texts foreach { text =>
        kmp.findFirst(text) should be (text.indexOf(word))
      }
    }
  }
}
