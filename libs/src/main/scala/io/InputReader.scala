package io

import java.io.{BufferedReader, InputStream, InputStreamReader}
import java.util.StringTokenizer
import lang._

class InputReader(val stream: InputStream) {
  private val reader = new BufferedReader(new InputStreamReader(stream), 32768)
  private var tokenizer: StringTokenizer = _

  def next(): String = {
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
