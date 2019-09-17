import org.scalatest.{FlatSpec, Matchers}
import lang._
import graph.EulerianTrail._

class EulerianTrailSpec extends FlatSpec with Matchers {
  "EulerianTrail" should "make eulerian trail" in {
    def makeGraph(n: Int, directed: Boolean, edges: (Int ,Int)*): G = {
      import scala.collection.mutable.ArrayBuffer
      val g = Array.fill[ArrayBuffer[Edge]](n)(ArrayBuffer())
      for {
        ((u, v), id) <- edges.zipWithIndex
      } {
        g(u-1) += Edge(v-1, id)
        if (!directed) g(v-1) += Edge(u-1, id)
      }
      g
    }
    val udg = makeGraph(4, directed = false, 2 -> 1, 3 -> 2, 4 -> 3, 2 -> 4)
    makeUDPath(udg, 4).map(_.map(_+1).toSeq) should be (Some(Seq(1, 2, 4, 3, 2)))

    val dg = makeGraph(4, directed = true, 2 -> 1, 3 -> 2, 4 -> 3, 2 -> 4)
    makeDPath(dg, 4).map(_.map(_+1).toSeq) should be (Some(Seq(2, 4, 3, 2, 1)))
  }
}
