import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File, PrintStream}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Tester {

  case class TestCase(name: String, in: String, out: String)

  def readTestCases(name: String): Seq[TestCase] = {
    val tests = ArrayBuffer[TestCase]()

    val file = getClass.getClassLoader.getResource(s"$name.txt")
    val txt = Source.fromURL(file).mkString
    var pos = 0
    while(txt != null) {
      val inStart = txt.indexOf("\uD83D\uDE3Ain", pos)
      if (inStart == -1) return tests
      val outStart = txt.indexOf("\uD83D\uDE3Aout", inStart)
      if (outStart == -1) throw new IllegalStateException("形式がおかしいです")
      val end = txt.indexOf("\uD83D\uDE3Aend", outStart)
      if (end == -1) throw new IllegalStateException("形式がおかしいです")
      val name = txt.substring(inStart+5, txt.indexOf('\n', inStart))
      val in = txt.substring(txt.indexOf('\n', inStart)+1, outStart)
      val out = txt.substring(txt.indexOf('\n', outStart)+1, end)
      tests += TestCase(name, in, out)
      pos = end+6
    }

    tests
  }

  def main(args: Array[String]): Unit = {
    val okAll = readTestCases(args(0)).zipWithIndex forall { case (test, i) =>
      import java.io.PrintWriter

      import Main.InputReader
      println(s"[${test.name}]")

      val out = new ByteArrayOutputStream()
      val in = new ByteArrayInputStream(test.in.getBytes("utf-8"))
      val pw = new PrintWriter(out)

      val s = System.nanoTime()

      val solver = new Main(pw, new InputReader(in))
      solver.solve()
      pw.flush()

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
