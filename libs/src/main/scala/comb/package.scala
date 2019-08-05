import lang._

package object comb {
  def inclusionExclusion(len: Int)(f: Int => Long): Long = {
    var res = 0L
    REP(1 << len) { set =>
      if (set > 0) {
        if (Integer.bitCount(set) % 2 == 0) res -= f(set)
        else res += f(set)
      }
    }
    res
  }
}
