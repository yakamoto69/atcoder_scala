package rmq

class BIT(n: Int) {
  private val N = {
    val a = Integer.highestOneBit(n)
    if (a == n) a else a << 1
  }
  private val bit = Array.ofDim[Int](N + 1)

  def sum(i: Int): Int = {
    var x = i
    var s = 0
    while(x > 0) {
      s += bit(x)
      x -= x & -x
    }
    s
  }

  def add(i: Int, a: Int): Unit = {
    var x = i
    while(x <= N) {
      bit(x) += a
      x += x & -x
    }
  }
}
