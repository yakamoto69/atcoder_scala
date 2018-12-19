import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen
import string._
import lang._
import testlang._

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

  def tupled[A, B](ga: Gen[A], gb: Gen[B]): Gen[(A, B)] = {
    ga.flatMap(a => gb.map(a -> _))
  }

  "longestSuffix" should "find longest suffix matched prefix" in {
    def char = Gen.oneOf('a', 'b')

    def genWord = for {
      n <- Gen.chooseNum(1, 10)
      word <- Gen.listOfN(n, char)
    } yield word.mkString


    def genArg = {
      for {
        w <- genWord
        arg <- Gen.listOf(tupled(Gen.chooseNum(0, w.length - 1), char))
      } yield (w, arg)
    }

    forAll(genArg) { case (word, args) =>
      def findLongestSuffix(s: String) = {
        var j = 0
        REP(s.length, 1) { i =>
          if (word.substring(0, i) == s.substring(s.length - i)) j = i
        }
        j
      }

      val kmp = new KMP(word)
      args foreach { case (matchedLen, nextChar) =>
        val s = word.substring(0, matchedLen) + nextChar
        System.err.println(s"$word $matchedLen $nextChar ${findLongestSuffix(s)}")
        kmp.longestSuffix(matchedLen, nextChar) should be (findLongestSuffix(s))
      }
    }
  }
}
