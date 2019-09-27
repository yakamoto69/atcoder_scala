import org.scalacheck.{Arbitrary, Gen}
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import lang._
import testlang._
import rmq.SegmentTree
import rmq.RangeUpdateTree
import SegmentTreeSpec._

class SegmentTreeSpec extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {
  "SegmentTree" should "accumulate every segment" in {
    forAll(genSteps(10, 50)) { case (n, steps) =>
      whenever(n > 0) {
        val t = new SegmentTree[Long](n, 0)((a, b) => a + b)
        val v = Array.ofDim[Int](n)

        def test(): Unit = {
          val cum = Array.ofDim[Long](n + 1)
          REP(n)(i => cum(i + 1) = cum(i) + v(i))

          def sum(l: Int, r: Int) = cum(r + 1) - cum(l)

          REP(n) { l =>
            l + 1 until n foreach { r =>
              withClue(s"query($l, ${r+1}): "){t.query(l, r + 1) should be(sum(l, r))}
            }
          }
        }

        // 引数が各々勝手にいじられるのでチェックしないといけない
        steps.filter(_.i < n) foreach { case Step(i, num) =>
          t.update(i, num)
          v(i) = num
          test()
        }
      }
    }
  }

  "RangeUpdateTree" should "update range value and query one value" in {
    val t = new RangeUpdateTree[Int](10, 0)(_+_)
    t.add(0, 10, 1)
    REP(10) { i =>
      t.query(i) should be (1)
    }
    t.add(1, 9, 10)
    t.query(0) should be (1)
    t.query(9) should be (1)
    REP(8, 1) { i =>
      t.query(i) should be (11)
    }
  }
}

object SegmentTreeSpec {
  case class Step(i: Int, num: Int)

  def genStep(n: Int, positive: Boolean) = for {
    i <- Gen.choose(0, n - 1)
    v <- if (positive) Gen.posNum[Int] else Arbitrary.arbInt.arbitrary
  } yield Step(i, v)

  def genSteps(maxN: Int, maxCnt: Int, positive: Boolean = false) = for {
    n <- genNum(maxN) //　別にnがでかいテストに意味がない。全範囲のテストが高いのでnを小さくする
    cnt <- genNum(maxCnt)
    steps <- Gen.listOfN(cnt, genStep(n, positive))
  } yield (n, steps)
}