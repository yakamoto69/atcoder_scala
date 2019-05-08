import java.io._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

import lang._

object InteractiveTester {

  import scala.util.Random

  val solverIn = new PipedInputStream()
  val testerOut = new PipedOutputStream()
  solverIn.connect(testerOut)
  val solverOut = new PipedOutputStream()
  val testerIn = new PipedInputStream()
  testerIn.connect(solverOut)
  val out = new PrintWriter(testerOut, true)
  val sc = new io.InputReader(testerIn)
  import sc._

  def main(args: Array[String]): Unit = {
    val okAll = dataSet.forall { dat =>
      val s = System.nanoTime()

      val fut = runSolver() // solveは別スレッドで
      val result = Try(test(dat)) // 判定はmainスレッドで
      Await.result(fut, Duration.Inf)


      val e = System.nanoTime()
      def show2(answer: Answer, success: Boolean) = {
        _show(success)
        debug(s"data: $dat, answer: $answer")
        success
      }

      def show(success: Boolean): Boolean = {
        _show(success)
        debug(s"data: $dat")
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
  case class Data(x: Int, m: Int, p: String)
  case class Answer(y: Int)
  val MAX_QUERY = 60

  def dataSet: Seq[Data] = {
    val rand = map(10){ _ =>
      val m = Random.nextInt(1000000000) + 1
      val n = Random.nextInt(30) + 1
      val p = map(n){ _ => if (Random.nextBoolean()) '1' else '0' }.mkString
      Data(Random.nextInt(m) + 1, m, p)
    }

    Seq(
      Data(3, 3, "0000000"),
      Data(3, 5, "10"),
      Data(24, 50, "0000000"),
      Data(24, 50, "011100101"),
      Data(1, 1000000000, "001010110010101110101010101011"),
      Data(32523423, 1000000000, "001010110010101110101010101011"),
      Data(1000000000, 1000000000, "001010110010101110101010101011")
    ) ++ rand
  }

  def runSolver() = {
    import ExecutionContext.Implicits.global
    Future {
      val pw = new PrintWriter(solverOut)
      new Main(pw, new Main.InputReader(solverIn)).solve()
    }
  }

  def test(dat: Data): Option[Answer] = {
    val n = dat.p.length
    out.println(s"${dat.m} $n")

    var cnt = 0
    while(true) {
      if (cnt >= MAX_QUERY) {
        debug("MAX_QUERY")
        return None
      }
      val y = ni()
      val sig = if (dat.p(cnt %  n) == '1') 1 else -1
      val ans = Integer.compare(dat.x, y) * sig
      debug(s"$y $ans cnt=${cnt+1}")
      out.println(ans)
      if (ans == 0) return Some(Answer(y))
//      nc() match {
//        case '?' =>
//          if (cnt >= MAX_QUERY) return None
//          val s = ns()
//          out.println(applyRule(s, dat.rule))
//
//        case '!' =>
//          val s = ns()
//          return Some(Answer(s))
//      }
      cnt += 1
    }

    None // 到達不可
  }

  def ok(data: Data, ans: Answer): Boolean = {
    data.x == ans.y
  }
}
