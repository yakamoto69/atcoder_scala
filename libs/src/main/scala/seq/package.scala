import mat._
import lang._
import math.max
import rmq.BIT

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
      val a: Array[Array[Long]] = Array(
        Array(1, 1),
        Array(1, 0)
      )
      val res = power(a, n - 1, mod)
      res(0)(0).toInt
  }


  def LIS(A: Array[Int]): Int = {
    def lowerBound(a: Array[Int], lst: Int, x: Int): Int = {
      def step(l: Int, h: Int): Int = {
        if (h - l == 1) h
        else {
          val mid = (l + h) / 2
          if (a(mid) >= x) step(l, mid)
          else step(mid, h)
        }
      }

      step(-1, lst)
    }
    val lis = Array.ofDim[Int](A.length)
    var p = 0
    REP(A.length) { i =>
      val ix = lowerBound(lis, p, A(i))
      lis(ix) = A(i)
      p = max(ix + 1, p)
    }
    p
  }

  def countTranpositions(p: Array[Int]) = {
    val bit = new BIT(p.length)
    var cnt = 0L
    REP_r(p.length) { i =>
      cnt += bit.sum(p(i))
      bit.add(p(i), 1)
    }
    cnt
  }
}
