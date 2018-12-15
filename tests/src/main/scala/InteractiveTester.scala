import java.io._
import java.util.StringTokenizer

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

object InteractiveTester {

  val solverIn = new PipedInputStream()
  val testerOut = new PipedOutputStream()
  solverIn.connect(testerOut)
  val solverOut = new PipedOutputStream()
  val testerIn = new PipedInputStream()
  testerIn.connect(solverOut)
  val out = new PrintWriter(testerOut, true)
  val sc = new InputReader(testerIn)
  System.setIn(solverIn)
  System.setOut(new PrintStream(solverOut))

  def main(args: Array[String]): Unit = {
    val okAll = dataSet.forall { dat =>
      val s = System.nanoTime()

      val fut = runSolver() // solveは別スレッドで
      val result = Try(test(dat)) // 判定はmainスレッドで
      Await.result(fut, Duration.Inf)


      val e = System.nanoTime()
      def show2(answer: Answer, success: Boolean) = {
        _show(success)
        System.err.println(s"data: $dat, answer: $answer")
        success
      }

      def show(success: Boolean): Boolean = {
        _show(success)
        System.err.println(s"data: $dat")
        success
      }

      def _show(success: Boolean) = {
        val msg = if (success) "Success" else "Failure"
        System.err.println(s"$msg, ${(e - s) / 1e6.toInt}ms")
      }

      result match {
        case Success(Some(answer)) if ok(dat, answer) => show2(answer, true)
        case Success(Some(answer)) if !ok(dat, answer) => show2(answer, false)
        case Success(None) => show(false)
        case Failure(t) =>
          t.printStackTrace(System.err)
          show(false)
      }
    }

    if (okAll) System.err.println("SUCCESS ALL!")
    else System.err.println("FAILURE!")
  }

  // こっから下を適時変えてね
  type Query = (Char, Int)
  type Data = Seq[Int]
  type Answer = Int
  val MAX_QUERY = 60

  def dataSet: Seq[Data] = Seq(
    Seq(1,2,1,2,3,4,3,2),
    Seq(1,2,3,2,1,0)
  )

  def runSolver() = {
    import ExecutionContext.Implicits.global
    Future {
      new Main().solve()
    }
  }

  def test(dat: Data): Option[Answer] = {
    out.println(dat.length)
    REP(MAX_QUERY) { _ =>
      val (tpe, x) = readQuery(dat)
      tpe match {
        case '?' =>
          out.println(dat(x - 1))

        case '!' =>
          return Some(x)

        case _ => throw new Exception(s"$tpe $x")
      }
    }

    // クエリー回数を超えた
    None
  }

  def ok(data: Data, x: Int): Boolean = {
    val n = data.length
    if (x == -1) {
      0 until n / 2 forall { i =>
        data(i) != data((i + n / 2) % n)
      }
    } else {
      val i = x - 1
      i >= 0 && i < n && {
        data(i) == data((i + n / 2) % n)
      }
    }
  }

  def readQuery(dat: Data): Query = {
    val tpe = nc()
    val x = ni()
    tpe match {
      case '?'=> if (x <= 0 || x > dat.length) throw new Exception(s"Out of range: $x")
      case '!'=> if (!(x == -1 || (x > 0 && x <= dat.length))) throw new Exception(s"Out of range: $x")
    }
    (tpe, x)
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

  def map[@specialized A: ClassTag](n: Int)(f: Int => A): Array[A] = {
    val res = Array.ofDim[A](n)
    REP(n)(i => res(i) = f(i))
    res
  }

  def sumL(as: Array[Int]): Long = {
    var s = 0L
    REP(as.length)(i => s += as(i))
    s
  }

  def cumSum(as: Array[Int]) = {
    val cum = Array.ofDim[Int](as.length + 1)
    REP(as.length) { i =>
      cum(i + 1) = cum(i) + as(i)
    }
    cum
  }
}
