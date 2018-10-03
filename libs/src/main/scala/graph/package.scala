
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import lang._
import math.min

package object graph {

  def bipartiteMatch(g: Array[ArrayBuffer[Int]]): Int = {
    val m = Array.fill[Int](g.length)(-1)
    val used = Array.ofDim[Boolean](g.length)

    def dfs(s: Int): Boolean = {
      case class Match(a: Int, b: Int)
      class Stacked(val v: Int, val matches: List[Match]) {
        var i = 0
      }
      val stack = mutable.Stack[Stacked]()
      used(s) = true
      stack.push(new Stacked(s, Nil))

      def realign(matches: List[Match]): Unit = {
        matches foreach { x =>
          m(x.a) = x.b
          m(x.b) = x.a
        }
      }

      while (stack.nonEmpty) {
        val stacked = stack.head
        val v = stacked.v
        val matches = stacked.matches
        val i = stacked.i
        stacked.i += 1
        if (i >= g(v).length) stack.pop()
        else {
          val u = g(v)(i)
          val w = m(u)
          if (w < 0) {
            realign(Match(v, u) :: matches)
            return true
          } else if (!used(w)) {
            used(w) = true
            stack.push(new Stacked(w, Match(v, u) :: matches))
          }
        }
      }

      false
    }

    var res = 0
    rep(g.length) { v =>
      if (m(v) < 0) {
        java.util.Arrays.fill(used, false)
        if (dfs(v)) res += 1
      }
    }
    res
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

  def packUGraph(n: Int, from: Array[Int], to: Array[Int]): Array[Array[Int]] = {
    val t = new Array[Array[Int]](n)
    val p = new Array[Int](n)
    val m = from.length
    rep(m)(i => p(from(i)) += 1)
    rep(m)(i => p(to(i)) += 1)
    rep(n)(i => t(i) = new Array(p(i)))
    rep(m) { i =>
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
    rep(m)(i => p(from(i)) += 1)
    rep(n)(i => t(i) = new Array(p(i)))
    rep(m) { i =>
      p(from(i)) -= 1
      t(from(i))(p(from(i))) = to(i)
    }
    t
  }

  def findCycle(n: Int, g: Array[Array[Int]]): Option[Array[Int]] = {
    val flag = Array.ofDim[Int](n)
    val ix = Array.ofDim[Int](n)
    val q = Array.ofDim[Int](n)

    def dfs(i: Int): Option[Array[Int]] = {
      def step(last: Int): Option[Array[Int]] =
        if (last < 0) None
        else {
          val v = q(last)

          flag(v) match {
            case 2 => step(last - 1)

            case 0 | 1 =>
              flag(v) = 1
              if (g(v).length == ix(v)) { // vの探索終わり
                flag(v) = 2
                step(last - 1)
              } else {  // dfs(u)
                val u = g(v)(ix(v))
                ix(v) += 1
                val next = last + 1
                flag(u) match {
                  case 0 =>
                    q(next) = u
                    step(next)

                  case 1 => // 閉路見つかった
                    val found = q.indexOf(u)
                    Some(java.util.Arrays.copyOfRange(q, found, next))

                  case _ => step(last)
                }
              }
          }
        }

      q(0) = i
      step(0)
    }

    def step(i: Int): Option[Array[Int]] =
      if (i == n) None
      else {
        if (flag(i) != 0) step(i + 1)
        else dfs(i) match {
          case None => step(i + 1)
          case Some(cycle) => Some(cycle)
        }
      }

    step(0)
  }

  /**
    * @return (depth, parent, queue)
    */
  def traceBfs(g: Array[Array[Int]]): (Array[Int], Array[Int], Array[Int]) = {
    val n = g.length
    val INF = 1e9.toInt + 10
    val q, p = Array.ofDim[Int](n)
    p(0) = -1
    val d = Array.fill[Int](n)(INF)
    d(0) = 0
    var cur = 0
    var last = 1
    while (cur < last) {
      val v = q(cur)
      rep(g(v).length) { i =>
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
    * Lowest Common Ancestor
    */
  {
    val NN = 100000
    val K = 20
    val g: Array[Array[Int]] = ???

    val (depth, parent, _) = traceBfs(g)
    parent(0) = 0
    val anc = Array.ofDim[Int](K, NN)
    rep(NN) { i =>
      anc(0)(i) = parent(i)
    }
    rep(anc.length - 1, 1) { k =>
      rep(NN) { i =>
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

      val d = min(depth(v), depth(u))
      val v0 = getParent(v, depth(v) - d)
      val u0 = getParent(u, depth(u) - d)
      if (v0 == u0) v0 // この時点で同じだったらダブリングで調べる必要がない
      else step(v0, u0, anc.length - 1)
    }
  }
}
