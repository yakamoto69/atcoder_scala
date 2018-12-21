package graph
import lang._
import scala.collection.mutable.ArrayBuffer

package object tree {
  /**
    * すでに直径、直径のノードがわかっている場合は、直径の半分だけ親をたどればいいのでもっと楽にできる
    * todo *注意* エッジが空のときに動かない
    * @return (木の中心１こor２個, 直径, 半径)
    */
  def centerOfTree(t: Array[Array[Int]]): (Array[Int], Int, Int) = {
    val n = t.length

    var cur = 0
    var p = 0
    val queue = Array.ofDim[Int](n)
    val degree = Array.ofDim[Int](n)
    val lvl = Array.ofDim[Int](n)
    REP(n) { i =>
      degree(i) = t(i).length
      if (degree(i) == 1) {
        queue(p) = i
        p += 1
      }
    }

    while(cur < p) {
      val v = queue(cur)
      cur += 1
      REP(t(v).length) { i =>
        val u = t(v)(i)

        // parentを探す
        if (degree(u) > 0) {
          degree(u) -= 1
          if (degree(u) == 1) {
            lvl(u) = lvl(v) + 1
            queue(p) = u
            p += 1
          }
        }
      }
      degree(v) -= 1
    }

    val c1 = queue(n - 1)
    if (n >= 2 && lvl(c1) == lvl(queue(n - 2))) {
      val c2 = queue(n - 2)
      (Array(c1, c2), lvl(c1) * 2 + 1, lvl(c1) + 1)
    } else {
      (Array(c1), lvl(c1) * 2, lvl(c1))
    }
  }

  def centersOfForest(g: Array[Array[Int]]): ArrayBuffer[(Array[Int], Int, Int)] = {
    val n = g.length

    var cur = 0
    var p = 0
    val queue = Array.ofDim[Int](n)
    val degree = Array.ofDim[Int](n)
    val lvl = Array.ofDim[Int](n)
    val last = Array.ofDim[Int](n) // 最後にエッジを削除したときの対になるノード
    val res = ArrayBuffer[(Array[Int], Int, Int)]()

    REP(n) { i =>
      degree(i) = g(i).length
      if (degree(i) == 1) {
        queue(p) = i
        p += 1

      // どこともつながっていないノードもツリーとみなす
      } else if (degree(i) == 0) {
        res += ((Array(i), 0, 0))
      }
    }

    while(cur < p) {
      val v = queue(cur)
      cur += 1
      var parent = -1
      REP(g(v).length) { i =>
        val u = g(v)(i)
        // parentを探す
        if (degree(u) > 0) parent = u
      }
      if (parent != -1) {
        val u = parent
        degree(u) -= 1
        if (degree(u) == 1) {
          lvl(u) = lvl(v) + 1
          queue(p) = u
          last(u) = v
          p += 1
        }
      } else {
        val c1 = v
        if (n >= 2 && lvl(c1) == lvl(last(v))) {
          val c2 = last(v)
          res += ((Array(c1, c2), lvl(c1) * 2 + 1, lvl(c1) + 1))
        } else {
          res += ((Array(c1), lvl(c1) * 2, lvl(c1)))
        }
      }
      degree(v) -= 1
    }

    res
  }
}
