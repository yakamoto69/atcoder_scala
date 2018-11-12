import lang._
import scala.util.Sorting
import math.{min, max}
import rmq.SegmentTree
import lang._

package object string {

  class KMP(word: String) {
    val kmp: Array[Int] = Array.ofDim[Int](word.length + 1)
    2 to word.length foreach { i =>
      var j = kmp(i - 1)
      var continues = true
      while(continues) {
        if (word(j) == word(i - 1)) {
          j += 1
          continues = false
        } else if (j == 0) continues = false
        else j = kmp(j)
      }
      kmp(i) = j
    }

    def findFirst(text: String): Int = {
      var j = 0
      rep(text.length) { i =>
        var continues = true
        while(continues) {
          if (word(j) == text(i)) {
            j += 1
            continues = false
          } else if (j == 0) continues = false
          else j = kmp(j)
        }
        if (j == word.length) return i - word.length + 1
      }
      -1
    }
  }

  /**
    * 大文字小文字が混ざってる場合はうまくいかないので注意
    */
  def suffixArray(s: String): Array[Int] = {
    val n = s.length

    val sa = Array.iterate(0, n + 1)(_ + 1)
    val rank, tmp = Array.ofDim[Int](n + 1)
    rep(n) { i =>
      rank(i) = s(i)
    }
    rank(n) = -1

    def lt(len: Int)(i: Int, j: Int): Boolean = {
      if (rank(i) != rank(j)) rank(i) < rank(j)
      else {
        // 長さが足りないってことは文字がないてことなので、辞書順で先になる
        val ri = if (i + len <= n) rank(i + len) else -1
        val rj = if (j + len <= n) rank(j + len) else -1
        ri < rj
      }
    }

    var len = 1
    while(len <= n) {
      // 今のランクとダブリングをつかってlen*2までの長さで並び替える
      Sorting.quickSort[Int](sa)(Ordering.fromLessThan(lt(len)))

      // 次のlen*2の長さで並び替えたsaからrankの値を計算する
      tmp(sa(0)) = 0
      rep(n, 1) { i =>
        tmp(sa(i)) = tmp(sa(i - 1)) + (if (lt(len)(sa(i - 1), sa(i))) 1 else 0) // 前の順位のものより大きい場合は+1する
      }

      Array.copy(tmp, 0, rank, 0, n + 1)
      len *= 2
    }
    sa
  }

  def longestCommonPrefix(s: String, sa: Array[Int]): (Array[Int], Array[Int]) = {
    val n = s.length
    val lcp = Array.ofDim[Int](n + 1)
    val rank = Array.ofDim[Int](n + 1)
    rep(n + 1) { i =>
      rank(sa(i)) = i
    }
    var h = 0
    rep(n) { i =>
      val j = sa(rank(i) - 1) // sa上で１個前
      h = max(0, h - 1)
      while(i + h < n && j + h < n && s(i + h) == s(j + h)) {
        h += 1
      }
      lcp(rank(i)) = h
    }
    (lcp, rank)
  }

  class LCPTree(s: String) {
    private val (lcp, rank) = longestCommonPrefix(s, suffixArray(s))

    private val t = new SegmentTree(s.length, Integer.MAX_VALUE / 2)(min)
    rep(s.length) { i =>
      t.update(i, lcp(i + 1))
    }

    def commonPrefix(i: Int, j: Int) = {
      val (l, r) = (min(rank(i), rank(j)), max(rank(i), rank(j)))
      t.query(l, r)
    }
  }
}
