import graph._
import graph.tree._
import lang._
import testlang._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {

  import org.scalacheck.Gen

  import scala.collection.JavaConverters._

  type Tree = (Int, List[(Int, Int)])
  val RT: Tree = (1, Nil)

  /**
    * @return (centers, diameter, radius, Tree)
    */
  def genTree: Gen[(Seq[Int], Int, Int, Tree)] = {
    for {
      f <- genBool
      r0 <- genNum(10)
      r = if (f) r0 else r0 - 1
      gened <- if (f) {
        genTwoCTree(r)
      } else {
        genOneCTree(r)
      }
    } yield (gened._2, if (f) 2 * r - 1 else 2 * r, r, gened._1)
  }

  def genTwoCTree(r: Int): Gen[(Tree, Seq[Int])] = {
    assert(r >= 0)
    for {
      t1 <- genOneCTree(r - 1)
      t2 <- genOneCTree(r - 1)
      (t, c2) = merge(t1._1, t2._1)
    } yield (t, Seq(0, c2))
  }

  def genOneCTree(r: Int): Gen[(Tree, Seq[Int])] = {
    assert(r >= 0)

    if (r == 0) {
      // ルートのみのとき
      ((1, Nil), Seq(0))
    } else {
      for {
        subCnt <- genNum(5) map (_ + 1) // 2以上
        subs <- Gen.sequence(map(subCnt)(_ => genTreeOf(r - 1)))
      } yield (subs.asScala.foldLeft(RT)(merge(_, _)._1), Seq(0))
    }
  }

  /**
    * rt = 0
    */
  def genTreeOf(depth: Int): Gen[Tree] = {
    assert(depth >= 0)
    def step(d: Int, parents: Array[Int], t: Tree): Gen[Tree] = {
      if (d == depth + 1) Gen.const(t)
      else {
        val n = t._1
        for {
          cnt <- genNum(5)
          newVs = map(cnt, n)(identity)
          newEs <- Gen.sequence(map(cnt, n) { v => Gen.oneOf(parents) map (v -> _) }) map (_.asScala.toList)
          res <- step(d + 1, newVs, (n + cnt, newEs ::: t._2))
        } yield res
      }
    }

    step(1, Array(0), RT)
  }

  /**
    * @return (Tree, t2側のrtの新しいid)
    */
  def merge(t1: Tree, t2: Tree): (Tree, Int) = {
    val n = t1._1
    val m = t2._1
    val edges = t1._2 ::: (0, n) :: t2._2.map(e => (e._1 + n, e._2 + n))
    ((n + m, edges), n)
  }

  "centerOfTree" should "calculate tree center and diameter" in {
    forAll(genTree) { case (centers, diameter, radius, tree) =>
      val (n, edges) = tree
      val (from, to) = edges.unzip
      val t = packUGraph(n, from.toArray, to.toArray)
      val (c, d, r) = centerOfTree(t)
      r should be(radius)
      d should be(diameter)
      c.toSet should be(centers.toSet)
    }
  }
}
