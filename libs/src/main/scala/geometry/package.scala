package object geometry {
  /**
    * [-π, π]
    */
  def angle(x1: Int, y1: Int, x2: Int, y2: Int): Double = {
    val sin = -y1 * x2 + x1 * y2  // perp dot product
    val cos = x1 * x2 + y1 * y2   // dot product
    Math.atan2(sin, cos)
  }
}
