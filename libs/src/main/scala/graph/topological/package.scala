package graph

import lang._

package object topological {
  /**
    * @return 閉路の場合はnull
    */
  def topologicalSort(g: Array[Array[Int]]): Array[Int] = {
    val n = g.length
    val L = Array.ofDim[Int](n)
    var ptr = 0
    val S = Array.ofDim[Int](n)
    var last, cur = 0
    val deg = Array.ofDim[Int](n)
    REP(n) { i =>
      REP(g(i).length) { j =>
        deg(g(i)(j)) += 1
      }
    }

    REP(n) { i =>
      if (deg(i) == 0) {
        S(last) = i
        last += 1
      }
    }

    while(cur < last) {
      val v = S(cur)
      cur += 1
      L(ptr) = v
      ptr += 1

      REP(g(v).length) { i =>
        val u = g(v)(i)
        deg(u) -= 1
        if (deg(u) == 0) {
          S(last) = u
          last += 1
        }
      }
    }

    // 閉路チェック
    REP(n) { i =>
      if (deg(i) > 0) return null
    }
    L
  }
}
