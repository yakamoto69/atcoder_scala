import lang._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import math.min

package object integer {

  type A = Long

  def gcd(a: A, b: A): A = {
    if (b == 0) a
    else gcd(b, a % b)
  }

  def gcd(as: Array[A]): A = {
    var res = as(0)
    REP(as.length - 1, 1) { i =>
      res = gcd(res, as(i))
    }
    res
  }

  /**
    * sa + tb = gcd(a, b) の (s, t) の組みを返す
    * @return (s, t)
    */
  def gcd_ext(a: A, b: A): (A, A) = {
    case class Bezout(r: A, s: A, t: A)
    def step(st0: Bezout, st1: Bezout): (A, A) = {
      if (st1.r == 0) {
        (st0.s, st0.t)
      } else {
        val q = st0.r / st1.r
        val r = st0.r - q * st1.r
        val s = st0.s - q * st1.s
        val t = st0.t - q * st1.t
        val st2 = Bezout(r, s, t)
        step(st1, st2)
      }
    }

    step(Bezout(a, 1, 0), Bezout(b, 0, 1))
  }

  /*
    * 10^18までのpowを配列で用意しておく
    */
  val pow10 = Array.ofDim[Long](19)
  pow10(0) = 1
  REP(pow10.length - 1)(i => pow10(i + 1) = pow10(i) * 10)

  val pow2 = Array.ofDim[Long](32)
  pow2(0) = 1
  REP(pow2.length - 1)(i => pow2(i + 1) = pow2(i) * 2)

  /**
    * @return (log10の整数値, 最大桁の値)
    */
  def log10(x: Long): (Int, Int) = {
    var a = x
    var i = 0
    while(a >= 10) {
      a /= 10
      i += 1
    }
    (i, a.toInt)
  }

  /**
    * log2して小数点切り捨てたもの
    */
  def log2(x: Int): Int = {
    assert(x > 0)
    var a = x
    var i = 0 // 何回2で割ったら1になるか
    while(a > 1) {
      i += 1
      a >>>= 1
    }
    i
  }

  /**
    * log2の小数点を切り上げたもの
    */
  def log2_ceil(x: Int): Int = {
    assert(x > 0)
    if (x == 1) {
      0
    } else {
      log2(x - 1) + 1
    }
  }

  /**
    *
    * 7 = (2 * 3) + 1 = (2 * (2 + 1)) + 1
    *
    */
  def powMod(x: Int, n: Int, m: Int): Int = {
    def step(x: Long, n: Int, stack: Long): Long = {
      n match {
        case 0 => stack
        case _ => step(x * x % m, n / 2, if (n % 2 == 1) stack * x % m else stack)
      }
    }
    step(x, n, 1).toInt
  }

  /**
    * O(√x)
    */
  def divisors(x: Int): Array[Int] = {
    val pre = ArrayBuffer[Int]()
    val post = ArrayBuffer[Int]()
    REP(math.sqrt(x).toInt, 1) { d =>
      if (x % d == 0) {
        pre += d
        if (d * d != x) post += x / d
      }
    }

    val res = Array.ofDim[Int](pre.length + post.length)
    REP(pre.length)(i => res(i) = pre(i))
    val preLen = pre.length
    val postLen = post.length
    REP(postLen)(i => res(i + preLen) = post(postLen - 1 - i))
    res
  }

  // Euler's Sieve
  {
    val NN = (1e7 + 5e6).toInt
    val prime = Array.ofDim[Int](1e6.toInt)
    val factor = Array.ofDim[Int](NN + 1)
    var pp = 0

    2 to NN foreach { i =>
      if (factor(i) == 0) {
        factor(i) = i
        prime(pp) = i
        pp += 1
      }

      def fill(p: Int): Unit = {
        if (p < pp && prime(p) * i <= NN) {
          factor(prime(p) * i) = prime(p)
          if (prime(p) != i)
            fill(p + 1)
        }
      }

      fill(0)
    }

    def factorize(x: Int)(fn: Int => Unit): Unit = {
      if (x > 1) {
        val f = factor(x)
        fn(f)
        factorize(x / f)(fn)
      }
    }

    def factorize2(x: Int) = {
      // NNが大きいときは増やしてね NN > 30000 => 6, NN > 500000 => 7 NN > 9000000 => 8
      val F = Array.ofDim[Int](8)
      var p = -1
      factorize(x) { f =>
        if (p == -1 || F(p) != f) {
          p += 1
          F(p) = f
        }
      }
      (F, p + 1)
    }
  }

  /**
    * *注意* 10^12以上ぐらいからは使わないこと
    */
  def factorize(n: A): mutable.Map[A, A] = {
    import scala.collection.mutable
    val res = mutable.Map[A, A]() withDefaultValue 0

    def minFactor(n0: A, rt: Int, i: Int): A = {
      if (i > rt) n0 // √n まで見つからなかったら n0は素数か1
      else if (n0 % i == 0) i
      else minFactor(n0, rt, i + 1)
    }

    def step(n0: A): Unit = {
      minFactor(n0, math.sqrt(n0).toInt, 2) match {
        case 1 =>
        case f =>
          res(f) = res(f) + 1
          step(n0 / f)
      }
    }

    step(n)
    res
  }

  def ceil(x: Long, div: Long) = {
    if (x < 0 && div >= 0 || x >= 0 && div < 0) {
      x / div
    } else {
      x / div + (if (x % div != 0) 1 else 0)
    }
  }

  def floor(x: Long, div: Long) = {
    if (x < 0 && div >= 0 || x >= 0 && div < 0) {
      x / div + (if (x % div != 0) -1 else 0)
    } else {
      x / div
    }
  }

  /**
    * 連続する0の最小の長さを返す
    * @param K bitを小さい方から何ビット調べるか
    */
  def minContinuousZero(bit: Int, K: Int): Int = {
    def step(k: Int, cnt: Int, ms: Int): Int = {
      k match {
        case K => if (cnt > 0) min(cnt, ms) else ms
        case _ =>
          if ((bit & 1 << k) == 0) {
            step(k + 1, if (cnt > 0) cnt + 1 else 1, ms) // 0
          } else {
            step(k + 1, -1, if (cnt > 0) min(cnt, ms) else ms) // 1
          }
      }
    }

    val ms = step(0, -1, K + 1)
    if (ms == K + 1) 0 else ms
  }
}
