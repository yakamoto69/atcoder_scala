import lang._

package object integer {

  def gcd(a: Int, b: Int): Int = {
    val r = a % b
    if (r == 0) b
    else gcd(b, r)
  }

  private case class Bezout(r: Int, s: Int, t: Int)

  /**
    * sa + tb = gcd(a, b) の (s, t) の組みを返す
    * @return (s, t)
    */
  def gcd_ext(a: Int, b: Int): (Int, Int) = {
    def step(st0: Bezout, st1: Bezout): (Int, Int) = {
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

  def permMod(n: Int, k: Int, m: Int): Int = {
    def step(n: Long, k: Long, v: Long): Long = {
      if (k == 0) v
      else step(n - 1, k - 1, n * v % m)
    }
    step(n, k, 1).toInt
  }

  def pow(x: Int, n: Int): Int = {
    n match {
      case 0 => 1
      case _ =>
        val r = pow(x * x, n / 2)
        if (n % 2 == 1) r * x else r
    }
  }

  def factorize(n: Int): Array[Int] = {
    def minFactor(n0: Int, rt: Int, i: Int): Int = {
      if (i > rt) n0 // √n まで見つからなかったら n0は素数か1
      else if (n0 % i == 0) i
      else minFactor(n0, rt, i + 1)
    }

    def step(n0: Int, acc: List[Int]): List[Int] = {
      minFactor(n0, math.sqrt(n0).toInt, 2) match {
        case 1 => acc
        case f => step(n0 / f, f :: acc)
      }
    }

    step(n, Nil).toArray
  }

  @specialized
  def permutation[A, B](a: Array[A], z: B)(f: (Array[A], B) => B): B = {
    def swap(i: Int, j: Int): Unit = {
      val temp = a(i)
      a(i) = a(j)
      a(j) = temp
    }

    def step(n: Int, acc: B): B = n match {
      case 1 => f(a, acc)
      case _ =>
        val acc2 = (0 until n - 1).foldLeft(acc) { (acc0, i) =>
          val acc1 = step(n - 1, acc0)
          if (n % 2 == 0) {
            swap(i, n - 1)
          } else {
            swap(0, n - 1)
          }
          acc1
        }
        step(n - 1, acc2)
    }

    step(a.length, z)
  }

  def comb(n: Int, k: Int): Seq[Array[Int]] = {
    def step(i: Int, k0: Int)(stack: () => Array[Int]): Seq[Array[Int]] = k0 match {
      case 0 => Seq(stack())
      case _ =>
        (i to n - k0) flatMap { j =>
          step(j + 1, k0 - 1) { () =>
            val a = stack()
            a(k - k0) = j
            a
          }
        }
    }

    step(0, k)(() => new Array[Int](k))
  }

  def perm[@specialized(Int, Long) A](n: Int, k: Int, z: A)(f: (A, Array[Int]) => A): A = {
    val p = Array.ofDim[Int](k)
    val used = Array.ofDim[Boolean](n)

    def step(i: Int, k0: Int, z0: A): A = k0 match {
      case 0 => f(z0, p)
      case _ =>
        (0 until n).foldLeft(z0){ (acc, j) =>
          if (used(j)) acc
          else {
            p(k - k0) = j
            used(j) = true
            val res = step(j + 1, k0 - 1, acc)
            used(j) = false
            res
          }
        }
    }

    step(0, k, z)
  }

  def lowerBound(a: Array[Long], x: Long): Int = {
    def step(l: Int, h: Int): Int = {
      if (h - l == 1) h
      else {
        val mid = l + (h - l) / 2
        if (a(mid) >= x) step(l, mid)
        else step(mid, h)
      }
    }
    step(-1, a.length)
  }

  def upperBound(a: Array[Long], x: Long): Int = {
    def step(l: Int, h: Int): Int = {
      if (h - l == 1) h
      else {
        val mid = l + (h - l) / 2
        if (a(mid) > x) step(l, mid)
        else step(mid, h)
      }
    }
    step(-1, a.length)
  }

  // nCk % MOD を求める。下準備で階乗Fと≡MODでの階乗の逆元Iを作る
  {
    val N = 10000
    val MOD = 1e9.toInt + 7

    def powMod(x: Int, n: Int, m: Int): Int = {
      def step(x: Long, n: Int, stack: Long): Long = {
        n match {
          case 0 => stack
          case _ => step(x * x % m, n / 2, if (n % 2 == 1) stack * x % m else stack)
        }
      }
      step(x, n, 1).toInt
    }

    val F = Array.ofDim[Long](N + 1)
    F(0) = 1
    rep(N) { i =>
      F(i + 1) = F(i) * (i + 1) % MOD
    }
    val I = Array.ofDim[Long](F.length)
    I(N) = powMod(F(N).toInt, MOD - 2, MOD)

    // x! = x(x-1)!
    // x!I(x!) ≡ (x-1)!I((x-1)!)
    // I((x-1)!) ≡ I(x!) * x   MODがでかいので(x-1)!はMODと素
    rep_r(N) { i =>
      I(i) = I(i + 1) * (i + 1) % MOD
    }

    def comb(n: Int, k: Int): Long = {
      if (n < k) 0
      else F(n) * I(n - k) % MOD * I(k) % MOD
    }
  }

}
