package graph

import scala.collection.mutable.ListBuffer
import lang._

class UnionFind(val n: Int) {
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

object UnionFind {

  def countDisjointSets(n: Int, V: Array[Int], U: Array[Int]): Int = {
    val uf = new UnionFind(n)
    var cnt = n
    rep(V.length) { i =>
      if (uf.find(U(i)) != uf.find(V(i))) {
        uf.unite(U(i), V(i))
        cnt -= 1
      }
    }
    cnt
  }

  def countDisjointSets(uf: UnionFind): Int = {
    var cnt = 0
    rep(uf.n) { i =>
      if (uf.find(i) == i) cnt += 1
    }
    cnt
  }

  def disjointSets(uf: UnionFind): Array[Array[Int]] = {
    val N = uf.n
    val cnt = Array.ofDim[Int](N)
    val disjoints = Array.ofDim[Array[Int]](N)
    rep(N) { i => cnt(uf.find(i)) += 1 }
    rep(N) { i => disjoints(i) = Array.ofDim(cnt(uf.find(i))) }
    rep(N) { i =>
      val id = uf.find(i)
      disjoints(id)(cnt(id) - 1) = i
      cnt(id) -= 1
    }
    disjoints
  }
}