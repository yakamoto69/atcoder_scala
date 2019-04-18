package utils

class KthMax(K: Int) {
  val heap = new LongMinHeap(K)

  def add(x: Long): Unit = {
    if (heap.length == K) {
      if (heap.min < x) {
        heap.removeMin()
        heap.add(x)
      }
    } else {
      heap.add(x)
    }
  }

  def toArray: Array[Long] = heap.toArray
}

class LongMinHeap(n: Int) {
  private val N = Integer.highestOneBit(n - 1) + 1 << 1
  private val heap = Array.ofDim[Long](N) // 1-indexed
  private var p = 0

  private def up(i: Int): Unit = {
    if (i >= 2) {
      val p = i / 2
      if (heap(i) < heap(p)) {
        val tmp = heap(p)
        heap(p) = heap(i)
        heap(i) = tmp
        up(p)
      }
    }
  }

  private def minchild(i: Int): Int = {
    if (i * 2 + 1 > p) {
      i * 2
    } else {
      if (heap(i * 2) < heap(i * 2 + 1)) i * 2
      else i * 2 + 1
    }
  }

  private def down(i: Int): Unit = {
    if (i * 2 <= p) {
      val mc = minchild(i)
      if (heap(i) > heap(mc)) {
        val tmp = heap(mc)
        heap(mc) = heap(i)
        heap(i) = tmp
        down(mc)
      }
    }
  }

  def add(x: Long): Unit = {
    p += 1
    heap(p) = x
    up(p)
  }

  def removeMin(): Unit = {
    heap(1) = heap(p)
    p -= 1
    down(1)
  }

  def min: Long = heap(1)
  def apply(i: Int): Long = heap(i + 1)
  def length: Int = p
  def toArray: Array[Long] = heap.slice(1, p + 1)
}
