package rmq

import scala.reflect.ClassTag

/**
  * @param n 個数 最大値じゃないぞ。
  * iの範囲は[0, n - 1]
  *
  * AがIntやLongのときは埋め込んでしまおう
  * type A = Int
  */
class SegmentTree[A: ClassTag](n: Int, zero: A)(f: (A, A) => A) {
  private val N = {
    val a = Integer.highestOneBit(n)
    if (a == n) a else a << 1
  }

  // zero == 0のときは初期化いらない
  private val dat: Array[A] = Array.fill(2 * N - 1)(zero)

  def update(i: Int, a: A): Unit = {
    assert(i < n)
    var k = N - 1 + i
    dat(k) = a
    while(k > 0) {
      k = (k - 1) / 2
      dat(k) = f(dat(2 * k + 1), dat(2 * k + 2))
    }
  }

  /**
    * [a, b)
    * [a, a) もok。この場合はzeroが返る
    */
  def query(a: Int, b: Int): A = {
    assert(a <= n && b <= n && a <= b)

    def step(k: Int, l: Int, r: Int): A = {
      if (r <= a || b <= l) {
        zero
      } else if (a <= l && r <= b) {
        dat(k)
      } else {
        val vl = step(k * 2 + 1, l, (l + r) / 2)
        val vr = step(k * 2 + 2, (l + r) / 2, r)
        f(vl, vr)
      }
    }

    if (a == b) zero
    else step(0, 0, N)
  }
}
