import lang._
import testlang._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scala.util.Sorting
import string._
import math.min
import org.scalacheck.Shrink

class SuffixArraySpec extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {

  "suffixArray" should "generates suffix array sorted alphabetically" in {
    forAll(Gen.alphaStr) { sArg =>
      val s = sArg.toLowerCase
      val n = s.length
      val sa = map(n + 1) { i =>
        i -> s.substring(i)
      }.sortBy(_._2) map (_._1)

      suffixArray(s).toSeq should be (sa.toSeq)
    }
  }

  "longestCommonPrefix" should "work well" in {
    forAll(Gen.alphaStr) { sArg =>
      val s = sArg.toLowerCase
      val n = s.length
      val sa = suffixArray(s)

      def commonPrefix(s1: String, s2: String) = {
        var i = 0
        while (i < min(s1.length, s2.length) && s1(i) == s2(i)) i += 1
        i
      }
      
      val expected = map(n + 1) { i =>
        if (i > 0) commonPrefix(s.substring(sa(i)), s.substring(sa(i - 1))) else 0
      }

      longestCommonPrefix(s, sa)._1.toSeq should be (expected.toSeq)
    }
  }
}
