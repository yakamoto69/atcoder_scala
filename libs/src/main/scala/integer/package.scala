import lang._
import scala.collection.mutable

package object integer {

  type A = Long

  def gcd(a: A, b: A): A = {
    if (b == 0) a
    else gcd(b, a % b)
  }

  def gcd(as: Array[A]): A = {
    var res = as(0)
    rep(as.length - 1, 1) { i =>
      res = gcd(res, as(i))
    }
    res
  }

  /*
    * 10^18までのpowを配列で用意しておく
    */
  val pow10 = Array.ofDim[Long](19)
  pow10(0) = 1
  rep(18)(i => pow10(i + 1) = pow10(i) * 10)

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
    * *注意* IntをLongにして使わないこと
    */
  def factorize(n: Int): mutable.Map[Int, Int] = {
    import scala.collection.mutable
    val res = mutable.Map[Int, Int]() withDefaultValue 0

    def minFactor(n0: Int, rt: Int, i: Int): Int = {
      if (i > rt) n0 // √n まで見つからなかったら n0は素数か1
      else if (n0 % i == 0) i
      else minFactor(n0, rt, i + 1)
    }

    def step(n0: Int): Unit = {
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

  // nCk % MOD を求める。下準備で階乗Fと≡MODでの階乗の逆元Iを作る
  {
    val NN = 10000
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

    val F = Array.ofDim[Long](NN + 1)
    F(0) = 1
    rep(NN) { i =>
      F(i + 1) = F(i) * (i + 1) % MOD
    }
    val I = Array.ofDim[Long](F.length)
    I(NN) = powMod(F(NN).toInt, MOD - 2, MOD)

    // x! = x(x-1)!
    // x!I(x!) ≡ (x-1)!I((x-1)!)
    // I((x-1)!) ≡ I(x!) * x   MODがでかいので(x-1)!はMODと素
    rep_r(NN) { i =>
      I(i) = I(i + 1) * (i + 1) % MOD
    }

    def comb(n: Int, k: Int): Long = {
      if (n < k) 0
      else F(n) * I(n - k) % MOD * I(k) % MOD
    }
  }

}
