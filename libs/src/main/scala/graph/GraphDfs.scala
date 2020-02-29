package graph

import lang._

class GraphDfs(g: Array[Array[Int]]) {
  private[this] val n = g.length
  private[this] val stack = Array.ofDim[Int](n)
  private[this] var s = 0
  private[this] val ix = Array.ofDim[Int](n)
  private[this] val used = Array.ofDim[Boolean](n)

  def apply(rt: Int = 0, enter: Int => Unit, exit: Int => Unit): Unit = {
    stack(s) = rt
    s += 1
    used(rt) = true

    while(s > 0) {
      val v = stack(s-1)
      val es = g(v)

      if (ix(v) == 0) {
        // dfsの開始部分
        enter(v)
      }

      if (ix(v) == es.length) {
        // dfsの終了部分
        exit(v)
        s -= 1
      }
      else {
        val u = es(ix(v))
        if (!used(u) && (s == 1 || u != stack(s-2))) { // stack覗くと親がわかる
          stack(s) = u
          s += 1
          used(u) = true
        }
        ix(v) += 1
      }
    }
  }

  def getUsed: Array[Boolean] = used
}

object GraphDfs {
  /**
    * @param g1 順グラフ
    * @param g2 逆グラフ
    * @return (強連結成分の個数, ノードがどの連結成分に属するか)
    */
  def stronglyConnectedComponents(g1: Array[Array[Int]], g2: Array[Array[Int]]): (Int, Array[Int]) = {
    val n = g1.length
    val vs = Array.ofDim[Int](n)
    var p = 0
    val dfs1 = new GraphDfs(g1)

    REP(n) { v =>
      if (!dfs1.getUsed(v)) {
        dfs1(v, _ => (), v => {
          vs(p) = v
          p += 1
        })
      }
    }
    val comp = Array.ofDim[Int](n)
    var k = 0
    val dfs2 = new GraphDfs(g2)
    REP_r(n) { i =>
      val v = vs(i)
      if (!dfs2.getUsed(v)) {
        dfs2(v, _ => (), v => {
          comp(v) = k
        })
        k += 1
      }
    }
    (k, comp)
  }
}