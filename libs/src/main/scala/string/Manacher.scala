package string

import lang._

object Manacher {

  // IntでもCharでも問題なし
  type A = Char

  class Manacher(S: Array[A]) {
    val R: Array[Int] = manacher(S)

    def manacher(S: Array[A]): Array[Int] = {
          val ex = '$'
//      val ex = 1e9.toInt

      def expand(S: Array[A]) = {
        val Sx = Array.ofDim[A](S.length * 2 + 1)
        REP(Sx.length) { i =>
          Sx(i) = if (i % 2 == 1) S(i / 2) else ex
        }
        Sx
      }

      manacher_core(expand(S))
    }

    def manacher_core(S: Array[A]): Array[Int] = {
      val N = S.length
      val R = Array.ofDim[Int](N)
      var i, j = 0
      while(i < N) {
        while(i - j >= 0 && i + j < N && S(i - j) == S(i + j)) j += 1
        R(i) = j
        var k = 1
        while(i - k >= 0 && i + k < N && k + R(i - k) < j) {
          R(i + k) = R(i - k)
          k += 1
        }
        i += k
        j -= k
      }

      R
    }

    // [l, r]
    def isPalindrome(l: Int, r: Int): Boolean = {
      // 拡張した文字列で回文なら、元の文字列でも回文
      val le = l * 2 + 1
      val re = r * 2 + 1
      val len = re - le + 1
      R((le + re) / 2) >= len / 2 + 1
    }
  }
}