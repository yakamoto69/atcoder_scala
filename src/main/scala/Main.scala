object Main {
  import java.io.{BufferedReader, InputStream, InputStreamReader}
  import java.util.StringTokenizer
  import scala.reflect.ClassTag

  def main(args: Array[String]): Unit = {
    val out = new java.io.PrintWriter(System.out)
    new Main(out, new InputReader(System.in)).solve()
    out.flush()
  }

  private[this] val oj = System.getenv("MY_DEBUG") == null
  def DEBUG(f: => Unit): Unit = {
    if (!oj){ f }
  }
  def debug(as: Array[Boolean]): Unit = if (!oj){ debug(as.map(x => if(x) "1" else "0").mkString) }
  def debug(as: Array[Int]): Unit = if (!oj){ debug(as.mkString(" ")) }
  def debug(as: Array[Long]): Unit =if (!oj){ debug(as.mkString(" ")) }
  def debugDim(m: Array[Array[Int]]): Unit = if (!oj){
    REP(m.length) { i =>
      debug(m(i))
    }
  }
  def debugDimFlip(m: Array[Array[Long]]): Unit = if (!oj){
    REP(m(0).length) { j =>
      REP(m.length) { i =>
        System.err.print(m(i)(j))
        System.err.print(" ")
      }
      System.err.println()
    }
  }
  def debug(s: => String): Unit = {
    if (!oj){ System.err.println(s) }
  }
  def isDebug[A](debug: => A, online: => A): A = {
    if (oj) online else debug
  }

  class InputReader(val stream: InputStream) {
    private[this] val reader = new BufferedReader(new InputStreamReader(stream), 32768)
    private[this] var tokenizer: StringTokenizer = _

    private[this] def next(): String = {
      while (tokenizer == null || !tokenizer.hasMoreTokens)
        tokenizer = new StringTokenizer(reader.readLine)
      tokenizer.nextToken
    }

    def nextInt(): Int = Integer.parseInt(next())
    def nextLong(): Long = java.lang.Long.parseLong(next())
    def nextChar(): Char = next().charAt(0)

    def ni(): Int = nextInt()
    def nl(): Long = nextLong()
    def nc(): Char = nextChar()
    def ns(): String = next()
    def ns(n: Int): Array[Char] = ns().toCharArray
    def na(n: Int, offset: Int = 0): Array[Int] = map(n)(_ => ni() + offset)
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
  }

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
    REP(to - from + 1, from)(f)
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
  def cumSum(as: Array[Int]): Array[Long] = {
    val cum = Array.ofDim[Long](as.length + 1)
    REP(as.length) { i =>
      cum(i + 1) = cum(i) + as(i)
    }
    cum
  }
}

object Workspace {
  import Main._
  import java.util.Arrays.sort

  import scala.collection.mutable
  import math.{abs, max, min}
  import mutable.ArrayBuffer
}

class Main(out: java.io.PrintWriter, sc: Main.InputReader) {
  import sc._
  import Main._
  import java.util.Arrays.sort

  import scala.collection.mutable
  import math.{abs, max, min}
  import mutable.ArrayBuffer
  import Workspace._

  // toIntとか+7とかするならvalにしろ
  final private[this] val MOD = 1000000007

  def solve(): Unit = {
    val N = ni()
    out.println(N)
  }
}