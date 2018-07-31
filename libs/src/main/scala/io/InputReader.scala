package io

import java.io.{BufferedReader, InputStream, InputStreamReader}
import java.util.StringTokenizer

class InputReader(val stream: InputStream) {
  val reader = new BufferedReader(new InputStreamReader(stream), 32768)
  var tokenizer: StringTokenizer = null

  def next: String = {
    while (tokenizer == null || !tokenizer.hasMoreTokens)
      tokenizer = new StringTokenizer(reader.readLine)
    tokenizer.nextToken
  }

  def nextInt(): Int = next.toInt
  def nextLong(): Long = next.toLong
  def nextChar(): Char = next.charAt(0)
}
