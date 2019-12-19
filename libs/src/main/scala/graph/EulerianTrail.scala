package graph
import lang._

object EulerianTrail {
  import scala.collection.mutable.ArrayBuffer

  case class Edge(u: Int, id: Int)
  type G = Array[ArrayBuffer[Edge]]

  def makeUDPath(g: G, m: Int): Option[Array[Int]] = {
    makePath(g, m) { g =>
      val n = g.length
      var odds = 0
      var s = g.indexWhere(_.nonEmpty)
      REP(n) { v =>
        val odd = g(v).length % 2 == 1
        if (odd) {
          s = v
          odds += 1
        }
      }

      (s, odds == 0 || odds == 2)
    }
  }

  def makeDPath(g: G, m: Int): Option[Array[Int]] = {
    makePath(g, m) { g =>
      val n = g.length

      var s = 0
      val in = Array.ofDim[Int](n)
      REP(n) { v =>
        if (g(v).nonEmpty) s = v
        REP(g(v).length) { i =>
          val u = g(v)(i).u
          in(u) += 1
        }
      }

      var plus, minus = 0
      var ok = true
      REP(n) { v =>
        // in - out
        if (in(v) == g(v).length + 1) {
          plus += 1
        } else if (in(v) + 1 == g(v).length) {
          minus += 1
          s += v // 収支マイナスの点から始める
        } else {
          //１こ以上ずれているとアウト
          ok &&= in(v) == g(v).length
        }
      }

      (s, ok && (plus == 0 && minus == 0 || plus == 1 && minus == 1))
    } map { path =>
      // ちょうど逆さまになっているので、有向グラフの場合はreverseしないといけない
      REP((m + 1)/2) { i =>
        val t = path(i)
        path(i) = path(m - i)
        path(m - i) = t
      }
      path
    }
  }

  private def makePath(g: G, m: Int)(f: G => (Int, Boolean)): Option[Array[Int]] = {
    val n = g.length
    val path = Array.ofDim[Int](m + 1)
    var pi = 0
    val used = Array.ofDim[Boolean](m)
    val ix = Array.ofDim[Int](n)

    val stack = Array.ofDim[Int](m + 1)
    var si = 0

    val (s, preCondition) = f(g)

    def dfs(s: Int): Unit = {
      stack(si) = s; si += 1
      while(si > 0) {
        val v = stack(si-1)
        while(ix(v) < g(v).length && used(g(v)(ix(v)).id)) ix(v) += 1
        if (ix(v) == g(v).length) {
          si -= 1
          path(pi) = v; pi += 1
        } else {
          used(g(v)(ix(v)).id) = true
          stack(si) = g(v)(ix(v)).u; si += 1; ix(v) += 1
        }
      }
    }

    if (!preCondition) {
      None
    } else {
      dfs(s)
      if (pi == m + 1) Some(path) else None
    }
  }

}
