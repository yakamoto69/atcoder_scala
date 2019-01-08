package object geometry {

  import scala.collection.mutable
  import scala.math.abs
  import lang._

  /**
    * [-π, π]
    */
  def angle(x1: Int, y1: Int, x2: Int, y2: Int): Double = {
    val sin = -y1 * x2 + x1 * y2  // perp dot product
    val cos = x1 * x2 + y1 * y2   // dot product
    Math.atan2(sin, cos)
  }

  case class Coord(x: Int, y: Int) {
    def ccw: Coord = Coord(-y, x)
    def +(o: Coord) = Coord(x + o.x, y + o.y)
  }

  def isSquare(p1: Coord, p2: Coord, p3: Coord, p4: Coord): Boolean = {
    val nums = mutable.Map[Long, Int]().withDefaultValue(0)
    val ps = Array(p1, p2, p3, p4)
    REP(4) { i =>
      i + 1 until 4 foreach { j =>
        val a = ps(i)
        val b = ps(j)
        val x = abs(a.x - b.x)
        val y = abs(a.y - b.y)
        val dist = x.toLong * x + y.toLong * y // ２乗の形を維持する
        nums(dist) = nums(dist) + 1
      }
    }
    nums.size == 2 && nums.toSeq.sortBy(_._1).map(_._2) == Seq(4, 2)
  }
}
