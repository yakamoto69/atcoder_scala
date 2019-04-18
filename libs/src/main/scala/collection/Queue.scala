package collection

import scala.reflect.ClassTag

class Queue[A: ClassTag](n: Int) {
  private val as = Array.ofDim[A](n)
  var p = 0
  var cur = 0

  def +=(a: A): Unit = {
    as(p) = a
    p += 1
  }

  def dequeue(): A = {
    val a = as(cur)
    cur += 1
    a
  }

  def head: A = as(cur)

  def apply(i: Int): A = as(cur + i)

  def length: Int = p - cur
  def isEmpty: Boolean = length == 0
  def nonEmpty: Boolean = !isEmpty

  def mkString(delimiter: String = ""): String = as.slice(cur, cur + length).mkString(delimiter)
}
