object GenData {
  def main(args: Array[String]): Unit = {
    import lang.map
    println(map(100000)(_ => 720720).mkString(" "))
  }
}
