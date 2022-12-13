object V06 {
  def main(args: Array[String]): Unit = {
    println(1 to 10)

    println(1 until 10)

    println((1 to 100).filter(_ % 3 == 0))

    println(flatten(List(List(1, 2, 3), List(3, 5, 6), List(), List(7, 8, 9))))

    println(sums(List(List(1, 2, 3), List(3, 5, 6), List(7, 8, 9), List())))

    println(removeEven(List(List(1, 2, 3), List(2, 4), List(3, 4, 5), List(7))))

    println(reverseAll(List("hello", "world")))

    println(removeAllDivBy3(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))))

    println(removeAllDivBy3AndRemoveLessThan5(List(List(4, 5, 6), List(7, 8, 9), List(10, 11, 12, 13, 14, 15, 16))))
  }

  def flatten(xs: List[List[Any]]): List[Any] = xs.reduce(_ ++ _)

  def sums(xs: List[List[Int]]): List[Int] = xs.map(x => if (x == Nil) List(0) else x).map(_.sum)

  def removeEven(xs: List[List[Int]]): List[List[Int]] = xs.map(_.filter(_ % 2 != 0)).filter(_ != Nil)

  def reverseAll(xs: List[String]): List[String] = xs.map(_.reverse)

  def removeAllDivBy3(xs: List[List[Int]]): List[List[Int]] = xs.map(_.filter(_ % 3 != 0))

  def removeAllDivBy3AndRemoveLessThan5(xs: List[List[Int]]): List[List[Int]] = removeAllDivBy3(xs).filter(_.size >= 5)
}
