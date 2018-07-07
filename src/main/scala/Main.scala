import java.io.{BufferedReader, InputStream, InputStreamReader, PrintWriter}
import java.util.StringTokenizer

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
    type Node = Int
    type Edge = (Node ,Long)
    val t1, t2 = map(n)(_ => ListBuffer.empty[Edge])
    rep(m) { _ =>
      val u, v = sc.nextInt() - 1
      val a, b = sc.nextLong()
      t1(u) += ((v, a))
      t1(v) += ((u, a))
      t2(u) += ((v, b))
      t2(v) += ((u, b))
    }

    def dijkstra(tree: Array[ListBuffer[Edge]], start: Int): Array[Long] = {
      val d = Array.fill[Long](n)(Long.MaxValue)
      type Visit = (Node, Long)
      val queue = mutable.PriorityQueue.empty[Visit](Ordering.by[Visit, Long](_._2).reverse)
      d(start) = 0
      queue.enqueue((start, 0))

      while(queue.nonEmpty) {
        val (v, c0) = queue.dequeue()
        if (d(v) == c0) {
          tree(v) foreach { case (u, c1) =>
            if (d(u) > c0 + c1) {
              d(u) = c0 + c1
              queue.enqueue((u, c0 + c1))
            }
          }
        }
      }

      d
    }

    val d1 = dijkstra(t1, s)
    val d2 = dijkstra(t2, t)

    val d = map(n){ i =>
      val j = n - 1 - i
      d1(j) + d2(j)
    }
    rep(n - 1) { i =>
      d(i + 1) = min(d(i + 1), d(i))
    }
    rep(n) { i =>
      val j = n - 1 - i
      val ans = 1000000000000000L - d(j)
      out.println(ans)
    }
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
}
