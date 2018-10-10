import utils._
import integer._
import math._

package object templates {

  type A = Long

  /**
    * lowerBound, upperBoundどっちもinclusiveなので、exclusiveを表現したいときは1ずらす
    */

  /**
    * [l, r)に含まれるx(i)のiの範囲を返す
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
}
