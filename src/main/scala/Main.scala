import java.io.{BufferedReader, InputStream, InputStreamReader, PrintWriter}
import java.util.{Comparator, StringTokenizer}

import scala.collection.mutable
import scala.util.Sorting
import math.{abs, max, min}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.ClassTag

object Main {
  val MOD = 1000000007
  val out = new PrintWriter(System.out)
  def solve(): Unit = {
    val sc = new InputReader(System.in)
    val n, m = sc.nextInt()
    val s, t = sc.nextInt() - 1
    val u, v, a, b = Array.ofDim[Int](m)
    rep(m) { i =>
      u(i) = sc.nextInt() - 1
      v(i) = sc.nextInt() - 1
      a(i) = sc.nextInt()
      b(i) = sc.nextInt()
    }
    val g1 = packWUGraph(n, u, v, a)
    val g2 = packWUGraph(n, u, v, b)

    val d1 = dijk(g1, s)
    val d2 = dijk(g2, t)

    val money = 1e15.toLong
    val ans = map(n)(i => money - d1(i) - d2(i))
    rep_r(n - 1)(i => ans(i) = max(ans(i), ans(i + 1)))
    rep(n)(i => out.println(ans(i)))
  }


  def main(args: Array[String]): Unit = {
    solve()
    out.flush()
  }


  class InputReader(val stream: InputStream) {
    private val reader = new BufferedReader(new InputStreamReader(stream), 32768)
    private var tokenizer: StringTokenizer = null

    def next: String = {
      while (tokenizer == null || !tokenizer.hasMoreTokens)
        tokenizer = new StringTokenizer(reader.readLine)
      tokenizer.nextToken
    }

    def nextInt(): Int = next.toInt
    def nextLong(): Long = next.toLong
    def nextChar(): Char = next.charAt(0)
  }

  private def rep(n: Int)(f: Int => Unit): Unit = (0 until n) foreach f
  private def rep_r(n: Int)(f: Int => Unit): Unit = (0 until n).reverse foreach f

  private def map[@specialized A: ClassTag](n: Int)(f: Int => A): Array[A] = {
    val res = Array.ofDim[A](n)
    (0 until n) foreach (i => res(i) = f(i))
    res
  }

  private implicit class ArrayOpts[A](val as: Array[A]) extends AnyVal {
    def maxByOpt[B: Ordering](f: A => B): Option[A] = {
      if (as.nonEmpty) Some(as.maxBy(f)) else None
    }

    def grpBy[K](f: A => K): mutable.Map[K, ArrayBuffer[A]] = {
      val map = mutable.Map.empty[K, ArrayBuffer[A]]
      rep(a => map.getOrElseUpdate(f(a), ArrayBuffer()) += a)
      map
    }

    def rep(f: A => Unit): Unit = {
      as.indices foreach (i => f(as(i)))
    }

    def sumBy[B](f: A => B)(implicit num: Numeric[B]): B = {
      var sum = num.zero
      rep(a => sum = num.plus(sum, f(a)))
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

  private implicit class IterableOpts[A](val as: Iterable[A]) extends AnyVal {
    def sumBy[B](f: A => B)(implicit num: Numeric[B]): B = {
      as.foldLeft(num.zero)((acc, a) => num.plus(acc, f(a)))
    }
  }

  type WUGraph = Array[Array[(Int, Int)]]
  /**
    * uwiのぱくり
    */
  def packWUGraph(n: Int, from: Array[Int], to: Array[Int], w: Array[Int]): WUGraph = {
    val g = new Array[Array[(Int, Int)]](n)
    val p = new Array[Int](n)
    val m = from.length
    rep(m)(i => p(from(i)) += 1)
    rep(m)(i => p(to(i)) += 1)
    rep(n)(i => g(i) = new Array(p(i)))
    rep(m) { i =>
      p(from(i)) -= 1
      g(from(i))(p(from(i))) = (to(i), w(i))
      p(to(i)) -= 1
      g(to(i))(p(to(i))) = (from(i), w(i))
    }
    g
  }

  def dijk(g: WUGraph, start: Int): Array[Long] = {
    val d = Array.fill[Long](g.length)(Long.MaxValue / 2)
    case class Visit(node: Int, cost: Long) extends Comparable[Visit] {
      override def compareTo(o: Visit): Int = java.lang.Long.compare(cost, o.cost)
    }
    val queue = new java.util.PriorityQueue[Visit]()
    d(start) = 0
    queue.add(Visit(start, 0))

    while(!queue.isEmpty) {
      val v = queue.poll()
      if (d(v.node) == v.cost) {
        g(v.node).rep { e =>
          val u = e._1
          val c = e._2
          val next = v.cost + c
          if (d(u) > next) {
            d(u) = next
            queue.add(Visit(u, next))
          }
        }
      }
    }

    d
  }
}
