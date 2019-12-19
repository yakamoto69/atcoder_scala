
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
  def traceBfs(g: Array[Array[Int]], rt: Option[Int] = Some(0)): (Array[Int], Array[Int], Array[Int]) = {
    val n = g.length
    val INF = 1e9.toInt + 10
    val q, p = Array.ofDim[Int](n)
    val d = Array.fill[Int](n)(INF)
    var cur, last = 0

    def bfs(rt: Int) {
      q(last) = rt
      last += 1
      p(rt) = -1
      d(rt) = 0

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
    }

    rt.map(bfs).getOrElse {
      REP(n) { v =>
        if (d(v) == INF) bfs(v)
      }
    }

    (d, p, q)
  }

  def isBipartite(g: Array[Array[Int]]): Boolean = {
    val (d, _, _) = traceBfs(g)
    g.indices forall { v =>
      g(v) forall { u =>
        d(v) % 2 != d(u) % 2 // 全エッジの両端の深さの偶奇が異なっている
      }
    }
  }
}
