import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import io.InputReader

package object lang {

  private val oj = System.getenv("ATCODER_DEBUG") == null

  def DEBUG(f: => Unit): Unit = {
    if (!oj){ f }
  }

  def debug(as: Array[Boolean]): Unit = DEBUG {
    debug(as.map(x => if(x) "1" else "0").mkString)
  }

  def debug(as: Array[Int]): Unit = DEBUG {
    debug(as.mkString(" "))
  }

  def debug(as: Array[Long]): Unit = DEBUG {
    debug(as.mkString(" "))
  }

  def debug(s: => String): Unit = DEBUG {
    System.err.println(s)
  }

  def debugNum(num: => Long): Unit = DEBUG {
    System.err.println(num)
  }

  val sc = new InputReader(System.in)
  def ni(): Int = sc.nextInt()
  def nl(): Long = sc.nextLong()
  def nc(): Char = sc.nextChar()
  def ns(): String = sc.next()
  def ns(n: Int): Array[Char] = ns().toCharArray
  def na(n: Int): Array[Int] = map(n)(_ => ni())
  def na2(n: Int, offset: Int = 0): (Array[Int], Array[Int]) = {
    val A1, A2 = Array.ofDim[Int](n)
    REP(n) { i =>
      A1(i) = ni() + offset
      A2(i) = ni() + offset
    }
    (A1, A2)
  }
  def nm(n: Int, m: Int): Array[Array[Int]] = {
    val A = Array.ofDim[Int](n, m)
    REP(n) { i =>
      REP(m) { j =>
        A(i)(j) = ni()
      }
    }
    A
  }
  def nal(n: Int): Array[Long] = map(n)(_ => nl())
  def nm_c(n: Int, m: Int): Array[Array[Char]] = map(n) (_ => ns(m))
  def REP(n: Int, offset: Int = 0)(f: Int => Unit): Unit = {
    var i = offset
    val N = n + offset
    while(i < N) { f(i); i += 1 }
  }
  def REP_r(n: Int, offset: Int = 0)(f: Int => Unit): Unit = {
    var i = n - 1 + offset
    while(i >= offset) { f(i); i -= 1 }
  }
  def TO(from: Int, to: Int)(f: Int => Unit): Unit = {
    REP(to - from + 1, from) { i =>
      f(i)
    }
  }

  def map[@specialized A: ClassTag](n: Int, offset: Int = 0)(f: Int => A): Array[A] = {
    val res = Array.ofDim[A](n)
    REP(n)(i => res(i) = f(i + offset))
    res
  }

  def sumL(as: Array[Int]): Long = {
    var s = 0L
    REP(as.length)(i => s += as(i))
    s
  }

  def cumSum(as: Array[Int]) = {
    val cum = Array.ofDim[Long](as.length + 1)
    REP(as.length) { i =>
      cum(i + 1) = cum(i) + as(i)
    }
    cum
  }

  implicit class ArrayOpts[A](val as: Array[A]) extends AnyVal {
    // todo Orderingだとboxing発生するので自作Orderを用意したい
    def maxByOpt[B: Ordering](f: A => B): Option[A] = {
      if (as.nonEmpty) Some(as.maxBy(f)) else None
    }

    def grpBy[K](f: A => K): mutable.Map[K, ArrayBuffer[A]] = {
      val map = mutable.Map.empty[K, ArrayBuffer[A]]
      REP(as.length)(i => map.getOrElseUpdate(f(as(i)), ArrayBuffer()) += as(i))
      map
    }

    def sumBy[B](f: A => B)(implicit num: Numeric[B]): B = {
      var sum = num.zero
      REP(as.length)(i => sum = num.plus(sum, f(as(i))))
      sum
    }

    def minByEx[B](f: A => B, ixRange: Range = as.indices)(implicit cmp: Ordering[B]): (A, B) = {
      limit(f, ixRange)(cmp.lt)
    }

    def maxByEx[B](f: A => B, ixRange: Range = as.indices)(implicit cmp: Ordering[B]): (A, B) = {
      limit(f, ixRange)(cmp.gt)
    }

    private def limit[B](f: A => B, ixRange: Range)(cmp: (B, B) => Boolean): (A, B) = {
      var limA = as(ixRange.head)
      var limB = f(limA)

      for (i <- ixRange.tail) {
        val a = as(i)
        val b = f(a)
        if (cmp(b, limB)) {
          limA = a
          limB = b
        }
      }
      (limA, limB)
    }
  }

  implicit class IterableOpts[A](val as: Iterable[A]) extends AnyVal {
    def sumBy[B](f: A => B)(implicit num: Numeric[B]): B = {
      as.foldLeft(num.zero)((acc, a) => num.plus(acc, f(a)))
    }
  }
}
