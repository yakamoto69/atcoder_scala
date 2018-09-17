object Main {
  def main(args: Array[String]): Unit = {
    val s = new Main()
    s.solve()
    s.out.flush()
  }
}

class Main {
  import java.io._
  import java.util.StringTokenizer

  import scala.collection.mutable
  import scala.util.Sorting
  import math.{abs, max, min}
  import mutable.{ArrayBuffer, ListBuffer}
  import scala.reflect.ClassTag

  val MOD = 1000000007
  val out = new PrintWriter(System.out)

  def solve(): Unit = {
    val N = ni()
  }


  class InputReader(val stream: InputStream) {
    private val reader = new BufferedReader(new InputStreamReader(stream), 32768)
    private var tokenizer: StringTokenizer = _

    def next(): String = {
      while (tokenizer == null || !tokenizer.hasMoreTokens)
        tokenizer = new StringTokenizer(reader.readLine)
      tokenizer.nextToken
    }

    def nextInt(): Int = next().toInt
    def nextLong(): Long = next().toLong
    def nextChar(): Char = next().charAt(0)
  }
  val sc = new InputReader(System.in)
  def ni(): Int = sc.nextInt()
  def nl(): Long = sc.nextLong()
  def nc(): Char = sc.nextChar()
  def ns(): String = sc.next()
  def ns(n: Int): Array[Char] = ns().toCharArray
  def na(n: Int): Array[Int] = map(n)(_ => ni())
  def nal(n: Int): Array[Long] = map(n)(_ => nl())
  def nm(n: Int, m: Int): Array[Array[Char]] = map(n) (_ => ns(m))
  def rep(n: Int, offset: Int = 0)(f: Int => Unit): Unit = {
    var i = offset
    val N = n + offset
    while(i < N) { f(i); i += 1 }
  }
  def rep_r(n: Int, offset: Int = 0)(f: Int => Unit): Unit = {
    var i = n - 1 + offset
    while(i >= offset) { f(i); i -= 1 }
  }

  def map[@specialized A: ClassTag](n: Int)(f: Int => A): Array[A] = {
    val res = Array.ofDim[A](n)
    rep(n)(i => res(i) = f(i))
    res
  }

  def sumL(as: Array[Int]): Long = {
    var s = 0L
    rep(as.length)(i => s += as(i))
    s
  }
}