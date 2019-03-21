import lang._

package object mat {
  /**
    * log(n)回しか再帰しないからOK
    */
  def power(a: Array[Array[Long]], n: Int, mod: Int): Array[Array[Long]] = {
    if (n == 1) a
    else {
      val q = n / 2
      val r = n % 2
      val b = power(mul(a, a, mod), q, mod)
      if (r == 1) mul(a, b, mod) else b
    }
  }

  def mul(a1: Array[Array[Long]], a2: Array[Array[Long]], mod: Int): Array[Array[Long]] = {
    assert(a1(0).length == a2.length)
    val r = a1.length
    val c = a2(0).length
    val len = a1(0).length
    val res = Array.ofDim[Long](r, c)
    REP(r) { i =>
      REP(c) { j =>
        var v = 0L
        REP(len) { k =>
          v += + a1(i)(k) * a2(k)(j)
          if (v > 7e18.toInt) v %= mod
        }
        res(i)(j) = if (v >= mod) v % mod else v
      }
    }
    res
  }

}
