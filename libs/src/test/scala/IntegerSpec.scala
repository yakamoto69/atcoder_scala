import org.scalatest._
import integer._
import lang._

class IntegerSpec extends FlatSpec with Matchers {
  "minContinuousZero" should "find min continuous zeros in bits such that '1001110' returns 1" in {
     minContinuousZero(BigInt("100110", 2).toInt, 6) should be (1)
     minContinuousZero(BigInt("10011", 2).toInt, 5) should be (2)
     minContinuousZero(BigInt("000000100011", 2).toInt, 12) should be (3)
     minContinuousZero(BigInt("0", 2).toInt, 12) should be (12)
     minContinuousZero(BigInt("1111", 2).toInt, 4) should be (0)
     minContinuousZero(BigInt("1111", 2).toInt, 10) should be (6)
     minContinuousZero(BigInt("0", 2).toInt, 10) should be (10)
  }

  "log2" should "return floored log2" in {
    REP(1e6.toInt, 1) { x =>
      1 << log2(x) should be (Integer.highestOneBit(x))
    }
  }

  "log2_ceil" should "return ceiled log2" in {
    REP(1e6.toInt, 1) { x =>
      val expected = if (x == Integer.highestOneBit(x)) log2(x) else log2(x << 1)
      log2_ceil(x) should be (expected)
    }
  }
}
