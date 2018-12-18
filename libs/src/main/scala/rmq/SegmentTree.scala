package rmq

/**
  * @param n 個数 最大値じゃないぞ。
  * iの範囲は[0, n - 1]
  */
class SegmentTree(n: Int, zero: A)(f: (A, A) => A) {
  private val N = {
    val a = Integer.highestOneBit(n)
    if (a == n) a else a << 1
  }
  private val dat: Array[A] = if (zero != 0){
    Array.fill(2 * N)(zero)
  } else {
    Array.ofDim(2 * N)
  }

  def update(i: Int, a: Int): Unit = {
    assert(i < n)
    var ix = i + N
    dat(ix) = a
    while(ix > 1) {
      dat(ix >> 1) = f(dat(ix), dat(ix ^ 1))
      ix >>= 1
    }
  }

  /**
    * [a, b]
    */
  def query(a: Int, b: Int): A = {
    assert(a <= n && b <= n)

    var res: A = zero
    var left = a + N
    var right = b + N

    while(left <= right) {
      if ((left & 1) == 1) res = f(res, dat(left))
      if ((right & 1) == 0) res = f(res, dat(right))
      left = (left + 1) >> 1 // 右の子供なら右の親に移動
      right = (right - 1) >> 1 // 左の子供なら左の親に移動
    }

    res
  }
}
