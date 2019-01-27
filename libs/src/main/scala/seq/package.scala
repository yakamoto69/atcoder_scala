import lang._
package object seq {
  /**
    * (fn+1) = (1 1) (fn  )
    * (fn  )   (1 0) (fn-1)
    *
    * f0 = 0
    * f1 = 1
    *
    *
    * (fn) = A(n-1) * (f1)
    * (fn-1)          (f0)
    *
    */
  def fib(n: Int, mod: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case _ =>
      val a = Array(
        Array(1, 1),
        Array(1, 0)
      )
      val res = power(a, n - 1, mod)
      res(0)(0)
  }

  /**
    * log(n)回しか再帰しないからOK
    */
  def power(a: Array[Array[Int]], n: Int, mod: Int): Array[Array[Int]] = {
    if (n == 1) a
    else {
      val q = n / 2
      val r = n % 2
      val b = power(mul(a, a, mod), q, mod)
      if (r == 1) mul(a, b, mod) else b
    }
  }

  def mul(a1: Array[Array[Int]], a2: Array[Array[Int]], mod: Int): Array[Array[Int]] = {
    assert(a1(0).length == a2.length)
    val r = a1.length
    val c = a2(0).length
    val len = a1(0).length
    val res = Array.ofDim[Int](r, c)
    REP(r) { i =>
      REP(c) { j =>
        REP(len) { k =>
          res(i)(j) = ((res(i)(j) + a1(i)(k).toLong * a2(k)(j)) % mod).toInt
        }
      }
    }
    res
  }
}
