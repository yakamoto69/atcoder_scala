import lang.rep

package object collection {
  def radixSort(f: Array[Int]): Array[Int] = radixSort(f, f.length)
  def radixSort(f1: Array[Int], n: Int): Array[Int] = {
    var f = f1
    var to = Array.ofDim[Int](n)

    val b = Array.ofDim[Int](65537)
    rep(n){ i => b(1 + (f(i) & 0xffff)) += 1 }
    rep(65536, 1){ i => b(i) += b(i - 1) }
    rep(n){ i =>
      val j = f(i) & 0xffff
      to(b(j)) = f(i)
      b(j) += 1
    }
    val temp = f
    f = to
    to = temp

    java.util.Arrays.fill(b, 0)
    rep(n){ i => b(1 + (f(i) >>> 16)) += 1 }
    rep(65536, 1){ i => b(i) += b(i - 1) }
    rep(n){ i =>
      val j = f(i) >>> 16
      to(b(j)) = f(i)
      b(j) += 1
    }
    to
  }

  /**
    * todo テストすれ
    * あとパフォーマンス調べろ
    */
  def radixSort(f: Array[Long]): Array[Long] = radixSort(f, f.length)
  def radixSort(f: Array[Long], n: Int): Array[Long] = {
    val x = Array(f, new Array[Long](n))
    val b = Array.ofDim[Int](65537)

    def step(k: Int, f: Int): Unit = {
      val t = f ^ 1
      rep(n){ i => b(1 + (x(f)(i) >>> k & 0xffff).toInt) += 1 }
      rep(65536, 1){ i => b(i) += b(i - 1) }
      rep(n){ i =>
        val j = (x(f)(i) >>> k & 0xffff).toInt
        x(t)(b(j)) = x(f)(i)
        b(j) += 1
      }
    }

    step(0, 0)
    java.util.Arrays.fill(b, 0)
    step(16, 1)
    java.util.Arrays.fill(b, 0)
    step(32, 0)
    java.util.Arrays.fill(b, 0)
    step(48, 1)

    x(0)
  }

  def sort(as: Array[Long]): Array[Long] = {
    val n = as.length
    val sorted = new java.util.PriorityQueue[Long](n)
    rep(n)(i => sorted.add(as(i)))
    rep(n)(i => as(i) = sorted.poll())
    as
  }
}
