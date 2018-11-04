package graph

import lang.rep

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

package object misc {
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
}
