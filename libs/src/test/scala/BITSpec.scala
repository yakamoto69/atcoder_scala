import lang._
import testlang._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scala.util.Sorting
import scala.collection.mutable.ArrayBuffer
import rmq.BIT
import rmq.BIT.ZippedCounter
import SegmentTreeSpec._
import utils._

class BITSpec extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {
  "BIT" should "accumulate every segment" in {
    forAll(genSteps(50, 100)) { case (n, steps) =>
      val t = new BIT(n)
      val v = Array.ofDim[Long](n)

      def test(): Unit = {
        val cum = Array.ofDim[Long](n + 1)
        REP(n)(i => cum(i + 1) = cum(i) + v(i))

        REP(n) { i =>
          withClue(s"sum($i): "){t.sum(i + 1) should be(cum(i + 1))}
        }

        withClue(s"sumAll: "){t.sumAll should be(cum(n))}

        REP(n) { l =>
          val r = l + genNum(n - l).sample.get
          withClue(s"query($l, $r): "){t.query(l, r) should be(cum(r) - cum(l))}
        }
      }

      // 引数が各々勝手にいじられるのでチェックしないといけない
      steps.filter(_.i < n) foreach { case Step(i, num) =>
        t.add(i, num)
        v(i) += num
        test()
      }
    }
  }

  "BIT.lowerBound" should "find min position which holds sum(i) >= x" in {
    forAll(genSteps(50, 100, positive = true)) { case (n, steps) =>
      val t = new BIT(n)
      val v = Array.ofDim[Long](n)

      def test(): Unit = {
        import scala.util.Random
        val cum = Array.ofDim[Long](n)
        cum(0) = v(0)
        REP(n - 1)(i => cum(i + 1) = cum(i) + v(i + 1))

        def testX(x: A): Unit = {
          withClue(s"find($x): [${cum.mkString(" ")}]") {
            t.lowerBound(x) should be(lowerBound(cum, x))
          }
        }

        // 全点調べる
        REP(n) { i =>
          testX(cum(i))
        }

        // ランダムで
        REP(n) { _ =>
          val x = if (cum(n - 1) > Integer.MAX_VALUE) Random.nextLong() else Random.nextInt(cum(n - 1).toInt)
          testX(x)
        }

        // 極端な値で
        testX(Long.MinValue)
        testX(Long.MaxValue)
      }

      steps foreach { case Step(i, num) =>
        t.add(i, num)
        v(i) += num
        test()
      }
    }
  }

  // countLtやれば他のcountも大丈夫だろう
  "ZippedCounter" should "countLt" in {
    forAll(Gen.listOf(Arbitrary.arbLong.arbitrary)) { points =>
      val as = points.toArray
      Sorting.quickSort(as)

      val cntr = new ZippedCounter(as)
      val added = ArrayBuffer[Long]()
      def cnt(f: Long => Boolean): Int = added count f

      // 全範囲調べるのは大変なので、追加した前後の境界と、それっぽい境界をしらべる
      def test(a: Long) = {
        for {
          x <- (-1 to 1 map (a + _)) ++ Seq(Long.MinValue, -1, 0, 1, Long.MaxValue)
        } cntr.countLt(x) should be (cnt(_ < x))
      }

      points foreach { a =>
        // 追加の前後にテストする
        test(a)
        cntr.add(a)
        added += a
        test(a)
      }
    }
  }

  "ZippedCounter" should "work when value is  duplicated" in {
    val as = Array[Long](1, 3, 3, 5)
    val z = new ZippedCounter(as)
    z.add(1)
    z.countLt(1) should be(0)
    z.countLt(2) should be(1)

    z.add(3)
    z.countLt(3) should be(1)
    z.countLt(4) should be(2)

    z.add(5)
    z.countLt(5) should be(2)
    z.countLt(6) should be(3)

    z.add(3)
    z.countLt(3) should be(1)
    z.countLt(4) should be(3)
  }
}
