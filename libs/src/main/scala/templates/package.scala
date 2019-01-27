import utils._
import integer._
import math.{min, max, abs}
import graph._
import lang._

package object templates {

  type A = Long

  /**
    * lowerBound, upperBoundどっちもinclusiveなので、exclusiveを表現したいときは1ずらす
    */

  /**
    * [l, r)に含まれるX(L)の下限, 最大のX(R)の上限を探す
    * l <= X(L), X(R) < r なので、LはlowerBoundで、Rは-1してupperBoundしてさらに-1
    * r - l < 0 の場合はそんな範囲はみつからなかったってこと
    * @param x ソート済み
    */
  def findZippedRange(x: Array[A], l: A, r: A): (Int, Int) = {
    (lowerBound(x, l), upperBound(x, r - 1) - 1)
  }

  /**
    * (l, r]
    */
  def findZippedRange2(x: Array[A], l: A, r: A): (Int, Int) = {
    (lowerBound(x, l + 1), upperBound(x, r) - 1)
  }

  /**
    * ピタゴラスの定理を使って斜辺とあと一辺の長さがわかっているときに、もう一辺の長さを計算する
    * Math.sqrt(c^2 - a^2) だと精度が落ちてしまうのでこうしないといけない
    */
  def calcOtherSide(c: Double, a: Double): Double = {
    Math.sqrt(c + a) * Math.sqrt(c - a)
  }

  def binSearch(ms: Int, mx: Int)(f: Int => Boolean): Int = {
    var high = mx + 1
    var low = ms - 1
    while(abs(high - low) > 1) {
      val mid = (high + low) / 2
      if (f(mid)) high = mid
      else low = mid
    }
    high
  }

  /**
    * 最大M個のグループに分割してグループ毎の配列に入れる
    */
  def grouped(N: Int, M: Int, g: Array[Int], v: Array[Int]): Array[Array[Int]] = {
    val cnt = Array.ofDim[Int](M)
    val C = Array.ofDim[Array[Int]](M)
    REP(N) { i =>
      cnt(g(i)) += 1
    }
    REP(M) { i =>
      C(i) = new Array(cnt(i))
    }
    REP_r(N) { i =>
      cnt(g(i)) -= 1
      C(g(i))(cnt(g(i))) = v(i)
    }
    C
  }

  /**
    * a, b は正の数。負だと不等式がひっくり返ってめんどう
    * ax + by = c を満たす (x, y)で、範囲が [L, R] を満たすペア
    */
  def gcd_ext_range(a: A, b: A, c: A, L: A, R: A)(f: (A, A) => Unit) = {
    val g = gcd(a, b)
    // xx, yyはc=gcdになるように両辺をc0で割ったときのx, yの値
    val (xx, yy) = gcd_ext(a, b)
    // 0がついている変数はgcdで割ったバージョン
    val a0 = a / g
    val b0 = b / g
    val c0 = c / g
    val x = c0 * xx // c0をかけて最初の式の解にする
    val y = c0 * yy

    // 最初の式に(x, y)と(x1, y1)を代入したものを変形していくと
    // a0(x1 - x) + b0(y1 - y) = 0 が得られる
    // a0, b0は素なので、
    // x1 = b0t + x, y1 = -a0t + y
    // L <= b0t + x <= R ->  [(L-x)/b0, (R-x)/b0]
    // L <= -a0t + y <= R -> [(R-y)/-a0, (L-y)/-a0] // -aで割るので不等式が反転してる
    val t_min = max(ceil(L - x, b0), ceil(R - y, -a0))
    val t_max = min(floor(R - x, b0), floor(L - y, -a0))
    t_min to t_max foreach { t =>
      val x1 = b0 * t + x
      val y1 = -a0 * t + y
      f(x1, y1)
    }
  }

  /**
    * nCk を全羅列する方法
    */
  def next_comb_bit(n: Int, k: Int)(f: Int => Unit) {
    if (k == 0) f(0)
    else {
      var flag = (1 << k) - 1
      while (flag < (1 << n)) {
        f(flag)
        val x = flag & -flag // 一番右の1
        val y = flag + x // 連続した1を0に、１個上のビットを1にする
        val z = flag & ~y // 連続した1部分
        flag = (z / x >> 1) | y // 連続した1を一つ減らして一番右に移動 + y
      }
    }
  }
  /**
    * 全ノードの組みのパスのsumをパスの長さが偶奇のもので分けて数える
    */
  def treeDP_sum_path_parity(N: Int, t: Array[Array[Int]]): Unit = {
    val (_, p, q) = traceBfs(t)
    val sum = Array.ofDim[Long](2, 2) // パスの長さ (Σlen, 個数)
    val dp = Array.ofDim[Long](N, 2, 2) // 深さ (Σdepth, 個数)
    REP_r(N) { i =>
      val v = q(i)
      // vを追加
      dp(v)(0)(1) += 1

      t(v) foreach { u =>
        if (p(v) != u) {
          // si = ciΣdi
          def cl(x: Int) = dp(v)(x)(1)
          def sl(x: Int) = dp(v)(x)(0)
          def cr(x: Int) = dp(u)(x)(1)
          def sr(x: Int) = dp(u)(x)(0) + cr(x) // 個数*1を足す

          // u側が１深くなるので偶奇が反転する
          REP(2) { xl =>
            REP(2) { xr =>
              // si = ciΣdi なので、
              // c1Σc2Σ(d1 + d2) = c2Σ(s1 + c1・d2) = s1・c2 + c1 + s2 -- パスのsum
              // c1Σc2Σ(1) = c1・c2 -- 組み合わせ数
              sum(xl ^ xr ^ 1)(0) += (sl(xl) * cr(xr) + sr(xr) * cl(xl))
              sum(xl ^ xr ^ 1)(1) += cl(xl) * cr(xr) // c1 * c2
            }
          }

          // dp(v)にdp(u)をマージする
          REP(2) { x =>
            // 1深くなるので反転する
            dp(v)(x)(0) += sr(x ^ 1)
            dp(v)(x)(1) += cr(x ^ 1)
          }
        }
      }
    }
  }
}
