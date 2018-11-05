import org.scalatest.{FlatSpec, Matchers}
import seq.fib
import lang._

class FibSpec extends FlatSpec  with Matchers {
  val MOD = 1e9.toInt + 7
  "fib" should "generates n(th) fibonacci sequence" in {
    val ans = map(10002)(fib(_, MOD))
    ans(0) should be (0)
    ans(1) should be (1)
    rep(10000, 2) { i =>
      ans(i) should be ((ans(i - 1) + ans(i - 2)) % MOD)
    }
  }
}
