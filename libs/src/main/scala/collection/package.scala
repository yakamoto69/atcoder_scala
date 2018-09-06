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
}
