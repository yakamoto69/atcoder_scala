package graph

import lang._
import math.min

/**
  * Lowest Common Ancestor
  * @param g packで作ったグラフでおきかえること
  * @param K pow(2,K) >= N-1 なのか？ダブリングの深さ
  */
class LCA(g: Array[Array[Int]], N: Int, K: Int){
  private[this] val ZERO = -1

  private[this] val (depth, parent, _) = traceBfs(g)
  // ルートを-1じゃなくてrtにする
  REP(N) { i =>
    if (depth(i) == 0) parent(i) = i
  }

  private[this] val anc = Array.ofDim[Int](K, N)
  REP(N) { i =>
    anc(0)(i) = parent(i)
  }
  REP(K - 1, 1) { k =>
    REP(N) { i =>
      anc(k)(i) = anc(k-1)(anc(k-1)(i))
    }
  }

  private def getParent(v: Int, k: Int, d: Int): Int = {
    if (d == 0) v
    else if (d % 2 == 1) getParent(anc(k)(v), k + 1, d / 2)
    else getParent(v, k + 1, d / 2)
  }

  /**
    * lca(v, ZERO) = v
    */
  def apply(v: Int, u: Int): Int = {
    def step(v0: Int, u0: Int, k: Int): Int = {
      if (k == -1) {
        anc(0)(v0) // １つ前の子を返したかったら(v0, u0)を返す
      } else if (anc(k)(v0) == anc(k)(u0)) {
        step(v0, u0, k - 1)
      } else {
        step(anc(k)(v0), anc(k)(u0), k - 1)
      }
    }

    if (v == ZERO) u
    else if (u == ZERO) v
    else {
      val d = min(depth(v), depth(u))
      val v0 = getParent(v, 0, depth(v) - d)
      val u0 = getParent(u, 0, depth(u) - d)
      if (v0 == u0) v0 // この時点で同じだったらダブリングで調べる必要がない
      else step(v0, u0, anc.length - 1)
    }
  }
}
