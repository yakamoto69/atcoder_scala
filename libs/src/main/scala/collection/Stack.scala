package collection

import scala.reflect.ClassTag

// primitve使う時はAをやめて直に埋め込め
class Stack[A: ClassTag](n: Int) {
  private val as = Array.ofDim[A](n)
  var p = 0

  def +=(a: A): Unit = {
    as(p) = a
    p += 1
  }

  def pop(): A = {
    p -= 1
    as(p)
  }

  def peek: A = as(p - 1)

  def apply(i: Int): A = as(i)

  def length: Int = p
  def isEmpty: Boolean = length == 0
  def nonEmpty: Boolean = !isEmpty

  def mkString(delimiter: String = ""): String = as.take(p).mkString(delimiter)
}
