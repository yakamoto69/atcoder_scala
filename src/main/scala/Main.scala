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


  def debug(as: Array[Boolean]): Unit = {
    System.err.println(as.map(x => if(x) "1" else "0").mkString)
  }

  def debug(as: Array[Int]): Unit = {
    System.err.println(as.mkString(" "))
  }

  def debug(as: Array[Long]): Unit = {
    System.err.println(as.mkString(" "))
  }

  def debug(s: String): Unit = {
    System.err.println(s)
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
  def REP(n: Int, offset: Int = 0)(f: Int => Unit): Unit = {
    var i = offset
    val N = n + offset
    while(i < N) { f(i); i += 1 }
  }
  def REP_r(n: Int, offset: Int = 0)(f: Int => Unit): Unit = {
    var i = n - 1 + offset
    while(i >= offset) { f(i); i -= 1 }
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
}