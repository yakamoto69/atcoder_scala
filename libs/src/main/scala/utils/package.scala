import lang._

package object utils {

  type A = Long

  /**
    * from, to共に絶対値でInteger, Longの最大の半分以下にすること
    */
  def findMin(from: A, to: A)(f: A => Boolean): Option[A] = {
    /**
      * (l, h]
      */
    def step(l: A, h: A): Option[A] = {
      if (h - l == 1) { // lがexclusiveなので h-l == 1 となった時点で`h`を返せばいい
        Some(h).filter(_ <= to) // 全部の範囲でfalseの場合 h = to+1 になっているはず
      } else {
        val mid = (l + h) / 2
        if (f(mid)) step(l, mid)
        else step(mid, h)
      }
    }

    step(from - 1, to + 1) // 配列の時と同じように`h`を+1する
  }

  def binarySearch(a: Array[A])(f: A => Boolean): Int = {
    /**
      * (l, h]
      */
    def step(l: Int, h: Int): Int = {
      if (h - l == 1) h // lがexclusiveなので h-l == 1 となった時点で`h`を返せばいい
      else {
        val mid = (l + h) / 2
        if (f(a(mid))) step(l, mid)
        else step(mid, h)
      }
    }

    // indexが-1の時が無限マイナスをさしてると思え
    step(-1, a.length)
  }

  // あえてコピペ
  // 要はcountLt
  def lowerBound(a: Array[A], x: A): Int = {
    def step(l: Int, h: Int): Int = {
      if (h - l == 1) h
      else {
        val mid = (l + h) / 2
        if (a(mid) >= x) step(l, mid)
        else step(mid, h)
      }
    }

    step(-1, a.length)
  }

  // 要はcountLe
  def upperBound(a: Array[A], x: A): Int = {
    def step(l: Int, h: Int): Int = {
      if (h - l == 1) h
      else {
        val mid = (l + h) / 2
        if (a(mid) > x) step(l, mid)
        else step(mid, h)
      }
    }

    step(-1, a.length)
  }
}
