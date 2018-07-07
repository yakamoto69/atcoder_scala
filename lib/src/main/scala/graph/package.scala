import scala.collection.mutable
import scala.collection.mutable.ListBuffer

package object graph {

  def dijk(g: Array[ListBuffer[(Int, Long)]], start: Int): Array[Long] = {
    val d = Array.fill[Long](g.length)(Long.MaxValue)
    type Visit = (Int, Long)
    val queue = mutable.PriorityQueue.empty[Visit](Ordering.by[Visit, Long](_._2).reverse)
    d(start) = 0
    queue.enqueue((start, 0))

    while(queue.nonEmpty) {
      val (v, c0) = queue.dequeue()
      if (d(v) == c0) {
        g(v) foreach { case (u, c1) =>
          if (d(u) > c0 + c1) {
            d(u) = c0 + c1
            queue.enqueue((u, c0 + c1))
          }
        }
      }
    }

    d
  }

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
}
