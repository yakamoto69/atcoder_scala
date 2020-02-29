import scala.math.{abs, max, min}

package object float {
  val EPS = 1e-9
  def lt(x: Double, y: Double) = x < y - EPS
  def le(x: Double, y: Double) = x < y + EPS
  def gt(x: Double, y: Double) = x > y + EPS
  def ge(x: Double, y: Double) = x > y - EPS
  def eq(x: Double, y: Double) = abs(x - y) < EPS

  def binSearch(l: Double, h: Double)(f: Double => Boolean): Double = {
    // ltをここでつかうとEPSがらみでおかしいことになって終了しない
    if (h - l < EPS * max(1, min(abs(l), abs(h)))) h
    else {
      val mid = (h + l) / 2.0
      if (f(mid)) binSearch(l, mid)(f)
      else binSearch(mid, h)(f)
    }
  }
}
