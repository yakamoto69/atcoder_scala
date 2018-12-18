import org.scalacheck.{Arbitrary, Gen}
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import lang._
import rmq.SegmentTree
import SegmentTreeSpec._

class SegmentTreeSpec extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {
  "SegmentTree" should "accumulate every segment" in {
    forAll(genSteps(10, 50)) { case (n, steps) =>
      whenever(n > 0) {
        val t = new SegmentTree(n, 0)((a, b) => a + b)
        val v = Array.ofDim[Int](n)

        def test(): Unit = {
          val cum = Array.ofDim[Long](n + 1)
          rep(n)(i => cum(i + 1) = cum(i) + v(i))

          def sum(l: Int, r: Int) = cum(r + 1) - cum(l)

          rep(n) { l =>
            l + 1 until n foreach { r =>
              withClue(s"query($l, $r): "){t.query(l, r) should be(sum(l, r))}
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
}

object SegmentTreeSpec {
  case class Step(i: Int, num: Int)

  def genStep(n: Int) = for {
    i <- Gen.choose(0, n - 1)
    v <- Arbitrary.arbInt.arbitrary
  } yield Step(i, v)

  def genSteps(maxN: Int, maxCnt: Int) = for {
    n <- Gen.choose(1, maxN) //　別にnがでかいテストに意味がない。全範囲のテストが高いのでnを小さくする
    cnt <- Gen.choose(1, maxCnt)
    steps <- Gen.listOfN(cnt, genStep(n))
  } yield (n, steps)
}