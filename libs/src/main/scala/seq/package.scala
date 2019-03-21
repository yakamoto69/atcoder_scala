import mat._
package object seq {
  /**
    * (fn+1) = (1 1) (fn  )
    * (fn  )   (1 0) (fn-1)
    *
    * f0 = 0
    * f1 = 1
    *
    *
    * (fn) = A(n-1) * (f1)
    * (fn-1)          (f0)
    *
    */
  def fib(n: Int, mod: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case _ =>
      val a: Array[Array[Long]] = Array(
        Array(1, 1),
        Array(1, 0)
      )
      val res = power(a, n - 1, mod)
      res(0)(0).toInt
  }
}
