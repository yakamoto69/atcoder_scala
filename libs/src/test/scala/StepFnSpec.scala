import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import lang._
import utils._
import testlang._


class StepFnSpec extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {
  "StepFunctions" should "behave as intended at border" in {
    import org.scalacheck.Gen

    val genBorder = for {
      n <- genNum(10) // そんないっぱいあっても意味ないだろう
      ls <- Gen.listOfN(n, genNum(100)) // 全範囲チェックするので広すぎるとこまる
    } yield ls.distinct.sorted.map(_.toLong).toArray // 重複は省く

    forAll(genBorder) { border =>
      val ceil = new CeilFn(border)
      val floor = new FloorFn(border)

      val min = (border.head - 10, border.head)
      val max = (border.last, border.last + 10)
      val steps = min +: (border zip border.drop(1)) :+ max

      // 各階段に0からNまでidがついている
      steps.zipWithIndex.foreach { case ((l, r), i) =>
        // ○---●
        l + 1 to r foreach { x =>
          ceil(x) should be(i)
        }

        // ●---○
        l until r foreach { x =>
          floor(x) should be(i)
        }
      }
    }
  }
}
