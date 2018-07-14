import scala.math.min

package object rmq {
  class SegmentTree(n: Int) {
    private val N = {
      val a = Integer.highestOneBit(n)
      if (a == n) a else a << 1
    }
    private val dat: Array[Int] = Array.fill(2 * N - 1)(Integer.MAX_VALUE)

    def update(i: Int, a: Int): Unit = {
      var k = N - 1 + i
      dat(k) = a
      while(k > 0) {
        k = (k - 1) / 2
        dat(k) = min(dat(2 * k + 1), dat(2 * k + 2))
      }
    }

    def query(a: Int, b: Int): Int = {
      def step(k: Int, l: Int, r: Int): Int = {
        if (r <= a || b <= l) {
          Integer.MAX_VALUE
        } else if (a <= l && r <= b) {
          dat(k)
        } else {
          val vl = step(k * 2 + 1, l, (l + r) / 2)
          val vr = step(k * 2 + 2, (l + r) / 2, r)
          min(vl, vr)
        }
      }

      step(0, 0, N)
    }
  }
}
