package rmq
import utils._

class BIT(n: Int, zero: A)(f: (A, A) => A) {
  def this(n: Int) = this(n, 0)(_ + _)

  private val N = {
    val a = Integer.highestOneBit(n)
    if (a == n) a else a << 1
  }
  private val bit = if (zero != 0) Array.fill[A](N + 1)(zero) else Array.ofDim[A](N + 1)

  /**
    * 0 index
    */
  def sum(i: Int): A = {
    var x = i + 1
    var s: A = 0
    while(x > 0) {
      s = f(s, bit(x))
      x -= x & -x
    }
    s
  }

  /**
    * 0 index
    */
  def add(i: Int, a: A): Unit = {
    var x = i + 1
    while(x <= N) {
      bit(x) = f(bit(x), a)
      x += x & -x
    }
  }
}

object BIT {
  /**
    * N + 1の範囲に分割する
    *
    * 例:
    * as = [3 5 6]
    * (, 3] => 0  (3, 5] => 1 (5, 6] => 2 (6, ) => 3
    *
    */
  class ZipperLB(as: Array[A]) {
    def apply(x: A): Int = {
      lowerBound(as, x)
    }
  }

  /**
    * まじめなBSTつくるの大変だからlesserThanをカウントできるだけのものを用意した
    * java.util.TreeSetじゃできない
    */
  class ZippedCounter(as: Array[A]) {
    val n = as.length
    val zip = new ZipperLB(as)
    val bit = new BIT(n + 1) // zipされたレンジはn + 1個になる
    var cnt = 0

    /**
      * @param x 必ずレンジの上限を追加しないいけない
      */
    def add(x: A): Unit = {
      val i = zip(x)
      assert(i < n && x == as(i))
      bit.add(i, 1)
      cnt += 1
    }

    def countLt(x: A): Int = {
      // レンジの上限の値しか存在しないので、ある値より小さい = より小さいレンジ と置き換えることができる
      val i = zip(x)
      if (i > 0) bit.sum(i - 1).toInt
      else 0
    }

    def countLe(x: A): Int = {
      countLt(x + 1)
    }

    def countGe(x: A): Int = {
      cnt - countLt(x)
    }

    def countGt(x: A): Int = {
      cnt - countLe(x)
    }
  }
}
