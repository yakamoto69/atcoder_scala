
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import lang._

package object graph {

  def findPath(s: Int, t: Int, N: Int, e: Array[Iterable[Int]]): Seq[Int] = {
    val INF = N + 10
    val d = Array.fill(N + 1)(INF)
    val q = mutable.PriorityQueue.empty[(Int, Int)](Ordering.by((_: (Int, Int))._1)) // (cost, 頂点)
    val prev = Array.fill(N + 1)(-1)

    d(s) = 0
    q.enqueue((0, s))

    while(q.nonEmpty) {
      val (cost, v) = q.dequeue()
      if (cost <= d(v)) {
        e(v).foreach { u =>
          if (d(u) > d(v) + 1) {
            d(u) = d(v) + 1
            prev(u) = v
            q.enqueue((d(u), u))
          }
        }
      }
    }

    def traceback(v: Int, end: Int, path: List[Int]): List[Int] = v match {
      case `end` => v :: path
      case _ => traceback(prev(v), end, v :: path)
    }

    traceback(t, s, Nil)
  }


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

  case class Edge(to: Int, weight: Int)
  type WUGraph = Array[Array[Edge]]
  /**
    * uwiのぱくり
    */
  def packWUGraph(n: Int, from: Array[Int], to: Array[Int], w: Array[Int]): WUGraph = {
    val g = new Array[Array[Edge]](n)
    val p = new Array[Int](n)
    val m = from.length
    rep(m)(i => p(from(i)) += 1)
    rep(m)(i => p(to(i)) += 1)
    rep(n)(i => g(i) = new Array(p(i)))
    rep(m) { i =>
      p(from(i)) -= 1
      g(from(i))(p(from(i))) = Edge(to(i), w(i))
      p(to(i)) -= 1
      g(to(i))(p(to(i))) = Edge(from(i), w(i))
    }
    g
  }

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
        rep(g(v.node).length) { i =>
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
}
