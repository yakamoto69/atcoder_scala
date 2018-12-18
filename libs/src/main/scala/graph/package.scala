
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import lang._
import math.min

package object graph {
  def dijk(g: WUGraph, start: Int): Array[Long] = {
    val d = Array.fill[Long](g.length)(Long.MaxValue / 2)
    case class Visit(node: Int, cost: Long) extends Comparable[Visit] {
      override def compareTo(o: Visit): Int = java.lang.Long.compare(cost, o.cost)
    }
    val queue = new java.util.PriorityQueue[Visit]()
    d(start) = 0
    queue.add(Visit(start, 0))

    while(!queue.isEmpty) {
      val v = queue.poll()
      if (d(v.node) == v.cost) {
        REP(g(v.node).length) { i =>
          val e = g(v.node)(i)
          val next = v.cost + e.weight
          if (d(e.to) > next) {
            d(e.to) = next
            queue.add(Visit(e.to, next))
          }
        }
      }
    }

    d
  }

  case class Edge(to: Int, weight: Int)
  type WUGraph = Array[Array[Edge]]
  /**
    * uwiのぱくり
    */
  def packWUGraph(n: Int, from: Array[Int], to: Array[Int], w: Array[Int]): WUGraph = {
    val g = new Array[Array[Edge]](n)
    val p = new Array[Int](n)
    val m = from.length
    REP(m)(i => p(from(i)) += 1)
    REP(m)(i => p(to(i)) += 1)
    REP(n)(i => g(i) = new Array(p(i)))
    REP(m) { i =>
      p(from(i)) -= 1
      g(from(i))(p(from(i))) = Edge(to(i), w(i))
      p(to(i)) -= 1
      g(to(i))(p(to(i))) = Edge(from(i), w(i))
    }
    g
  }

  /**
    * @param n ノード数
    */
  def packUGraph(n: Int, from: Array[Int], to: Array[Int]): Array[Array[Int]] = {
    val t = new Array[Array[Int]](n)
    val p = new Array[Int](n)
    val m = from.length
    REP(m)(i => p(from(i)) += 1)
    REP(m)(i => p(to(i)) += 1)
    REP(n)(i => t(i) = new Array(p(i)))
    REP(m) { i =>
      p(from(i)) -= 1
      t(from(i))(p(from(i))) = to(i)
      p(to(i)) -= 1
      t(to(i))(p(to(i))) = from(i)
    }
    t
  }

  def packDGraph(n: Int, from: Array[Int], to: Array[Int]): Array[Array[Int]] = {
    val t = new Array[Array[Int]](n)
    val p = new Array[Int](n)
    val m = from.length
    REP(m)(i => p(from(i)) += 1)
    REP(n)(i => t(i) = new Array(p(i)))
    REP_r(m) { i => // 順序維持する
      p(from(i)) -= 1
      t(from(i))(p(from(i))) = to(i)
    }
    t
  }

  /**
    * @return (depth, parent, queue)
    */
  def traceBfs(g: Array[Array[Int]], rt: Int = 0): (Array[Int], Array[Int], Array[Int]) = {
    val n = g.length
    val INF = 1e9.toInt + 10
    val q, p = Array.ofDim[Int](n)
    q(0) = rt
    p(rt) = -1
    val d = Array.fill[Int](n)(INF)
    d(rt) = 0
    var cur = 0
    var last = 1
    while (cur < last) {
      val v = q(cur)
      REP(g(v).length) { i =>
        val u = g(v)(i)
        if (d(u) == INF) {
          d(u) = d(v) + 1
          p(u) = v
          q(last) = u
          last += 1
        }
      }
      cur += 1
    }
    (d, p, q)
  }

  /**
    * @return (depth, parent, tour)
    */
  def traceDfs(g: Array[Array[Int]], rt: Int = 0): (Array[Int], Array[Int], Array[Int]) = {
    val n = g.length
    val stack = mutable.Stack[Int]()
    val ix = Array.ofDim[Int](n)
    val tour = Array.ofDim[Int](n)
    val depth = Array.ofDim[Int](n)
    val parent = Array.ofDim[Int](n)
    parent(rt) = -1
    var p = 0

    tour(p) = rt
    p += 1
    stack.push(rt)

    while(stack.nonEmpty) {
      val v = stack.top
      val es = g(v)
      if (ix(v) == es.length) stack.pop()
      else {
        val u = es(ix(v))
        if (u != parent(v)) {
          parent(u) = v
          depth(u) = depth(v) + 1
          stack.push(u)
          tour(p) = u
          p += 1
        }
        ix(v) += 1
      }
    }

    (depth, parent, tour)
  }

  def isBipartite(g: Array[Array[Int]]): Boolean = {
    val (d, _, _) = traceBfs(g)
    g.indices forall { v =>
      g(v) forall { u =>
        d(v) % 2 != d(u) % 2 // 全エッジの両端の深さの偶奇が異なっている
      }
    }
  }

  /**
    * Lowest Common Ancestor
    * @param g packで作ったグラフでおきかえること
    */
  def LCA(g: Array[Array[Int]]){
    val NN = 100000 // 要素数
    val K = 20 // 2^(K-1) >= NN なのか？ダブリングの深さ
    val ZERO = -1

    val (depth, parent, _) = traceBfs(g)
    parent(0) = 0

    val anc = Array.ofDim[Int](K, NN)
    REP(NN) { i =>
      anc(0)(i) = parent(i)
    }
    REP(anc.length - 1, 1) { k =>
      REP(NN) { i =>
        anc(k)(i) = anc(k-1)(anc(k-1)(i))
      }
    }

    def getParent(v: Int, d: Int): Int = {
      def step(v0: Int, k: Int, d0: Int): Int = {
        if (d0 == 0) v0
        else if (d0 % 2 == 1) step(anc(k)(v0), k + 1, d0 / 2)
        else step(v0, k + 1, d0 / 2)
      }

      step(v, 0, d)
    }

    /**
      * lca(v, ZERO) = v
      */
    def lca(v: Int, u: Int): Int = {
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
        val v0 = getParent(v, depth(v) - d)
        val u0 = getParent(u, depth(u) - d)
        if (v0 == u0) v0 // この時点で同じだったらダブリングで調べる必要がない
        else step(v0, u0, anc.length - 1)
      }
    }
  }
}
