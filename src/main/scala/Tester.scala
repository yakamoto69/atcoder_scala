import java.io._

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Tester {

  case class TestCase(in: String, out: String)

  def readTestCases(name: String): Seq[TestCase] = {
    var i = 0
    val in = Array.fill[ArrayBuffer[String]](2)(ArrayBuffer())
    val tests = ArrayBuffer[TestCase]()

    def addTest(): Unit = {
      tests += TestCase(in(0).mkString("\n"), in(1).mkString("\n"))
      in foreach (_.clear())
    }

    val file = new File(getClass.getClassLoader.getResource(s"$name.txt").getFile)
    val lines = Source.fromFile(file).getLines().toList
    lines foreach { l =>
      if (l.isEmpty || l.startsWith("//")) {
        if (in(i).nonEmpty) {
          i ^= 1
          if (i == 0) addTest()
        }
      } else {
        in(i) += l
      }
    }

    if (i == 0) throw new IllegalStateException("outで終わっていない")
    else addTest()

    tests
  }

  def main(args: Array[String]): Unit = {
    val okAll = readTestCases(args(0)).zipWithIndex forall { case (test, i) =>
      println(s"Test${i+1}")

      val out = new ByteArrayOutputStream()
      val po = new PrintStream(out)
      val in = new ByteArrayInputStream(test.in.getBytes("utf-8"))

      val s = System.nanoTime()
      scala.Console.withOut(po) {
        scala.Console.withIn(in) {
          val solver = new Main()
          solver.solve()
        }
      }
      po.flush()
      val e = System.nanoTime()

      val actual = out.toString("utf-8").trim
      val ok = actual == test.out.trim
      val msg = if (ok) "Success" else "Failure"
      println(actual)
      println(s"$msg, ${(e - s)/1e6.toInt}ms")
      println()

      ok
    }

    if (okAll) println("SUCCESS ALL!")
    else println("FAILURE!")
  }
}
