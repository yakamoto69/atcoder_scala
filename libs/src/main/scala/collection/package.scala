import lang.rep

package object collection {

  import scala.reflect.ClassTag

  /**
    * 正の値のみのときだけ
    */
  def radixSort(f: Array[Int]): Array[Int] = radixSort(f, f.length)
  def radixSort(as: Array[Int], n: Int): Array[Int] = {
    val n = as.length
    val xs = Array(as, new Array[Int](n))
    val b = Array.ofDim[Int](65537)

    def step(k: Int, x: Int): Unit = {
      rep(n){ i => b(1 + (xs(x)(i) >>> k & 0xffff)) += 1 }
      rep(65536, 1){ i => b(i) += b(i - 1) }
      rep(n){ i =>
        val j = xs(x)(i) >>> k & 0xffff
        xs(x ^ 1)(b(j)) = xs(x)(i)
        b(j) += 1
      }
    }

    step(0, 0)
    java.util.Arrays.fill(b, 0)
    step(16, 1)

    as
  }

  def radixSort[A: ClassTag](as: Array[A])(f: A => Int): Array[A] = {
    val n = as.length
    val xs = Array(as, new Array[A](n))
    val b = Array.ofDim[Int](65537)

    def step(k: Int, x: Int): Unit = {
      rep(n){ i => b(1 + (f(xs(x)(i)) >>> k & 0xffff)) += 1 }
      rep(65536, 1){ i => b(i) += b(i - 1) }
      rep(n){ i =>
        val j = f(xs(x)(i)) >>> k & 0xffff
        xs(x ^ 1)(b(j)) = xs(x)(i)
        b(j) += 1
      }
    }

    step(0, 0)
    java.util.Arrays.fill(b, 0)
    step(16, 1)

    as
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

    (0 until 4) foreach { i =>
      if (i > 0) java.util.Arrays.fill(b, 0)
      step(16 * i, i % 2)
    }

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
