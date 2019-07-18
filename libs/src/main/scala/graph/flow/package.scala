package graph

import scala.collection.mutable.ArrayBuffer
import scala.math.min
import lang._

package object flow {

  type A = Long
  type Graph = Array[ArrayBuffer[Edge]]
  case class Edge(v: Int, w: A)
  class Edge2(val to: Int, var cap: A, val rev: Int)
  def maxFlow(baseG: Graph, s: Int, t: Int, inf: A): A = {
    val n = baseG.length
    val level = Array.ofDim[Int](n)
    val iter = Array.ofDim[Int](n)

    val g = Array.fill[ArrayBuffer[Edge2]](n)(ArrayBuffer())
    def addEdge(v: Int, e: Edge): Unit = {
      g(v) += new Edge2(e.v, e.w, g(e.v).size)
      g(e.v) += new Edge2(v, 0, g(v).size - 1)
    }
    REP(n) { v =>
      REP(baseG(v).length) { j =>
        addEdge(v, baseG(v)(j))
      }
    }

    def bfs(s: Int): Unit = {
      val queue = new java.util.ArrayDeque[Int]()
      level(s) = 0
      queue.add(s)
      while(!queue.isEmpty) {
        val v = queue.poll()
        REP(g(v).length) { i =>
          val e = g(v)(i)
          if (e.cap > 0 && level(e.to) < 0) {
            level(e.to) = level(v) + 1
            queue.add(e.to)
          }
        }
      }
    }

    def dfs(v: Int, t: Int, f: A): A = {
      if (v == t) {
        f
      } else {
        while(iter(v) < g(v).length) {
          val e = g(v)(iter(v))
          if (e.cap > 0 && level(v) < level(e.to)) {
            val d = dfs(e.to, t, min(f, e.cap))
            if (d > 0) {
              e.cap -= d
              g(e.to)(e.rev).cap += d
              return d
            }
          }
          iter(v) += 1
        }
        0
      }
    }

    var flow: A = 0
    var continues = true
    while(continues) {
      import java.util
      util.Arrays.fill(iter, 0)
      util.Arrays.fill(level, -1)
      bfs(s)
      debug(level)
      if (level(t) < 0) {
        continues = false
      } else {
        var f: A = 0
        while({f = dfs(s, t, inf); f > 0}) {
          flow = min(inf, flow + f)
        }
      }
    }

    if (flow == inf) -1 else flow
  }
}
