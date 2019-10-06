package comb

import lang._

// nCk % MOD を求める。下準備で階乗Fと≡MODでの階乗の逆元Iを作る
class Comb(N: Int, MOD: Int) {
  def powMod(x: Int, n: Int, m: Int): Int = {
    def step(x: Long, n: Int, stack: Long): Long = {
      n match {
        case 0 => stack
        case _ => step(x * x % m, n / 2, if (n % 2 == 1) stack * x % m else stack)
      }
    }
    step(x, n, 1).toInt
  }

  private[this] val F = Array.ofDim[Long](N + 1)
  F(0) = 1
  REP(N) { i =>
    F(i + 1) = F(i) * (i + 1) % MOD
  }
  private[this] val I = Array.ofDim[Long](F.length)
  I(N) = powMod(F(N).toInt, MOD - 2, MOD)

  // x! = x(x-1)!
  // x!I(x!) ≡ (x-1)!I((x-1)!)
  // I((x-1)!) ≡ I(x!) * x   MODがでかいので(x-1)!はMODと素
  REP_r(N) { i =>
    I(i) = I(i + 1) * (i + 1) % MOD
  }

  def comb(n: Int, k: Int): Long = {
    if (n < k) 0
    else F(n) * I(n - k) % MOD * I(k) % MOD
  }

  def rev(x: Int): Long = {
    I(x) * F(x - 1) % MOD
  }

  /**
    * nのグループからk回重複ありで選ぶ組み合わせ数
    * n - 1のしきりとkの○で考える
    */
  def H(n: Int, k: Int): Long = {
    comb(n + k - 1, k)
  }

  /**
    * private[this]をつかってるのでgetterが必要
    * @return (F, I)
    */
  def get: (Array[Long], Array[Long]) = (F, I)
}
