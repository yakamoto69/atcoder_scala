import lang.REP
import scala.util.Sorting

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
      REP(n){ i => b(1 + (xs(x)(i) >>> k & 0xffff)) += 1 }
      REP(65536, 1){ i => b(i) += b(i - 1) }
      REP(n){ i =>
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

  def reverse(as: Array[Int]): Unit = {
    REP(as.length / 2) { i =>
      val t = as(i)
      as(i) = as(as.length - 1 - i)
      as(as.length - 1 - i) = t
    }
  }

  def radixSort[A: ClassTag](as: Array[A])(f: A => Int): Array[A] = {
    val n = as.length
    val xs = Array(as, new Array[A](n))
    val b = Array.ofDim[Int](65537)

    def step(k: Int, x: Int): Unit = {
      REP(n){ i => b(1 + (f(xs(x)(i)) >>> k & 0xffff)) += 1 }
      REP(65536, 1){ i => b(i) += b(i - 1) }
      REP(n){ i =>
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
    * あとパフォーマンス調べろ
    */
  def radixSort(f: Array[Long]): Array[Long] = radixSort(f, f.length)
  def radixSort(f: Array[Long], n: Int): Array[Long] = {
    val x = Array(f, new Array[Long](n))
    val b = Array.ofDim[Int](65537)

    def step(k: Int, f: Int): Unit = {
      val t = f ^ 1
      REP(n){ i => b(1 + (x(f)(i) >>> k & 0xffff).toInt) += 1 }
      REP(65536, 1){ i => b(i) += b(i - 1) }
      REP(n){ i =>
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

  def sort(a: Array[Long]): Array[Long] = {
    val n = a.length
    REP(n) { i =>
      val j = scala.util.Random.nextInt(n - i) + i
      val tmp = a(i)
      a(i) = a(j)
      a(j) = tmp
    }
    Sorting.quickSort(a)
    a
  }
}
