package graph

import scala.collection.mutable.ListBuffer

class UnionFind(n: Int) {
  private val par = Array.ofDim[Int](n)
  par.indices foreach (i => par(i) = i)
  private val rank = Array.ofDim[Int](n)

  def find(x: Int): Int = {
    val stack = ListBuffer[Int]()
    def step(x: Int): Int = {
      if (par(x) == x) x
      else {
        stack += x
        step(par(x))
      }
    }

    val res = step(x)
    stack foreach (i => par(i) = res)
    res
  }

  def unite(x: Int, y: Int): Unit = {
    val x1 = find(x)
    val y1 = find(y)
    if (x1 != y1) {
      if (rank(x1) < rank(y)) {
        par(x1) = y1
      } else {
        par(y1) = x1
        if (rank(x1) == rank(y1)) rank(x1) += 1
      }
    }
  }
}
