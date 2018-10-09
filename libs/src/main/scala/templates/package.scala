import utils._

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
}
