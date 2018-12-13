package utils

import lang._

// todo test
/**
  * DPなんかでKの個数を維持しつつ区間の最大を探す時に使う
  * DP上で１個前の状態からとるなどするときはmax(i-1)のようにその時のインデックスをちゃんと渡さないといけないのに注意
  */
class SlideMax(k: Int) {
  case class Entry(i: Int, v: A)

  val queue = new java.util.ArrayDeque[Entry]()

  def nonEmpty(i: Int): Boolean = {
    maintenance(i)
    !queue.isEmpty
  }

  def max(i: Int): A = {
    maintenance(i)
    queue.peek().v
  }

  def add(i: Int, x: A): Unit = {
    maintenance(i)

    // 条件逆さまにすると最小になる
    while(!queue.isEmpty && queue.peekLast().v <= x) queue.removeLast()
    queue.addLast(Entry(i, x))
  }

  def maintenance(i: Int): Unit = {
    while (!queue.isEmpty && k < i - queue.peek().i + 1) queue.removeFirst()
  }
}

object SlideMax {
  def example(N: Int, X: Int, K: Int, A: Array[Int]) {
    val dp = Array.fill[SlideMax](X + 1)(new SlideMax(K))
    dp(0).add(0, 0)

    REP(N, 1) { i =>
      val a = A(i - 1)
      REP_r(X) { x =>
        // １個前の状態を見るとにi-1してるところに注意
        if (dp(x).nonEmpty(i - 1)) dp(x + 1).add(i, dp(x).max(i - 1) + a)
      }
    }
  }
}