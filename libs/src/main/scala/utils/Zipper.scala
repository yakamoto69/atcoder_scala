package utils

import scala.collection.mutable

class Zipper {
  type A = Long
  var gen = 0
  val id = mutable.HashMap[A, Int]()

  def get(x: A): Int = {
    id.getOrElseUpdate(x, {
      val i = gen
      gen += 1
      i
    })
  }
}
