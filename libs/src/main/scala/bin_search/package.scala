import lang._

package object bin_search {
  type A = Int

  /**
    * 単調増加
    * (l, h] 方式
    */
  def findMin(f: A => Boolean, min: A, max: A): A = {
    var l = min - 1
    var h = max
    while(h - l > 1) {
      val x = (h + l) / 2
      if (f(x)) h = x
      else l = x
    }
    h
  }

  /**
    * 単調減少
    * [l, h) 方式
    */
  def findMax(f: A => Boolean, min: A, max: A): A = {
    var l = min
    var h = max + 1
    while(h - l > 1) {
      val x = (h + l) / 2
      if (f(x)) l = x
      else h = x
    }
    l
  }


  /**
    * インタラクティブのようなクエリーがぎりぎりのときはこっちを使う
    * 単調増加
    * [l, h] 方式
    */
  def findMin2(f: A => Boolean, min: A, max: A): A = {
    var l = min
    var h = max
    while(h != l) {
      val x = (h + l) / 2
      if (f(x)) h = x
      else l = x + 1
    }
    h
  }

  /**
    * インタラクティブのようなクエリーがぎりぎりのときはこっちを使う
    * 単調減少
    */
  def finxMax2(f: A => Boolean, min: A, max: A): A = {
    def g(x: Int) = f(max - x)
    max - findMin2(g, 0, max)
  }
}
