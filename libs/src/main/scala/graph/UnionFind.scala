package graph

import lang._

class UnionFind(val n: Int) {
  private val par = Array.ofDim[Int](n)
  REP(n) (i => par(i) = i)
  val rank: Array[Int] = Array.fill[Int](n)(1) // 集合の要素数
  private val visits = Array.ofDim[Int](n) // 訪れた場所をfind毎に用意するのがもったいないのでつかいまわす

  def find(x: Int): Int = {
    var ptr = 0
    def step(x: Int): Int = {
      if (par(x) == x) x
      else {
        visits(ptr) = x
        ptr += 1
        step(par(x))
      }
    }

    val res = step(x)
    REP(ptr){ i => par(visits(i)) = res }
    res
  }

  def unite(x: Int, y: Int): Int = {
    val x1 = find(x)
    val y1 = find(y)
    if (x1 == y1) x1
    else {
      if (rank(x1) < rank(y)) {
        par(x1) = y1
        y1
      } else {
        par(y1) = x1
        if (rank(x1) == rank(y1)) rank(x1) += rank(y1)
        x1
      }
    }
  }

  /**
    * xを解決する必要がないときは直にrankをみる
    */
  def cntNodes(x: Int): Int = rank(find(x))
}

object UnionFind {

  def countDisjointSets(n: Int, V: Array[Int], U: Array[Int]): Int = {
    val uf = new UnionFind(n)
    var cnt = n
    REP(V.length) { i =>
      if (uf.find(U(i)) != uf.find(V(i))) {
        uf.unite(U(i), V(i))
        cnt -= 1
      }
    }
    cnt
  }

  def countDisjointSets(uf: UnionFind): Int = {
    var cnt = 0
    REP(uf.n) { i =>
      // 同じかどうかだけを見るので、find使ってどの集合に属するかを解決する必要がない
      if (uf.par(i) == i) cnt += 1
    }
    cnt
  }

  /**
    * 配列のsetIdにあたる場所にその集合の要素が入っている。取り込まれた集合にはnullが入っている
    */
  def disjointSets(uf: UnionFind): Array[Array[Int]] = {
    val N = uf.n
    val cnt = Array.ofDim[Int](N)
    val disjoints = Array.ofDim[Array[Int]](N)
    REP(N) { i => cnt(uf.find(i)) += 1 }
    REP(N) { i =>
      if (cnt(i) > 0) disjoints(i) = Array.ofDim(cnt(i))
    }
    REP(N) { i =>
      val id = uf.find(i)
      disjoints(id)(cnt(id) - 1) = i
      cnt(id) -= 1
    }
    disjoints
  }
}