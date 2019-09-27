package rmq

import scala.reflect.ClassTag

/**
  * @param n 個数 最大値じゃないぞ。
  * iの範囲は[0, n - 1]
  *
  * AがIntやLongのときは埋め込んでしまおう
  * type A = Int
  */
class RangeUpdateTree[A: ClassTag](n: Int, zero: A)(f: (A, A) => A) {
  private val N = {
    val a = Integer.highestOneBit(n)
    if (a == n) a else a << 1
  }
  private val dat: Array[A] = if (zero != 0){
    Array.fill(2 * N)(zero)
  } else {
    Array.ofDim(2 * N)
  }

  /**
    * [l, r)
    */
  def add(l: Int, r: Int, a: A): Unit = {
    assert(l < n && r <= n)

    var left = l + N
    var right = r - 1 + N

    while(left <= right) {
      if ((left & 1) == 1) dat(left) = f(dat(left), a)
      if ((right & 1) == 0) dat(right) = f(dat(right), a)
      left = (left + 1) >> 1 // 右の子供なら右の親に移動
      right = (right - 1) >> 1 // 左の子供なら左の親に移動
    }
  }

  def query(i: Int): A = {
    assert(i < n)

    var ix = N + i
    var res: A = zero

    while(ix >= 1) {
      res = f(res, dat(ix))
      ix >>= 1
    }

    res
  }
}
