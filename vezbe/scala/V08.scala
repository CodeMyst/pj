object V08 {
  def main(args: Array[String]): Unit = {
    // TODO: test everything...
  }

  /**
   * Find all points that are closer than the specified distance to the center point.
   */
  private def pointsWithinDistance(points: List[(Double, Double)], center: (Double, Double), maxDist: Double): List[(Double, Double)] =
    points.filter(dist(_, center) < maxDist)

  private def dist(a: (Double, Double), b: (Double, Double)): Double =
    Math.sqrt(Math.pow(b._1 - a._1, 2) + Math.pow(b._2 - a._2, 2))

  private def weirdJoin(xs: List[String]): String =
    xs.flatMap(_.split(" ")).reduce(_ + ", " + _)

  private def weirdNumbers(xs: Array[Int]): Array[Int] =
    xs.map(x => if (x % 2 == 0) x + 1 else x * x).map(flipNumber).filter(_ % 2 != 0)

  private def flipNumber(num: Int): Int = num match {
    case x if x < 10 => x
    case _ => (num % 10) * 10 + flipNumber(num / 10)
  }

  private def weirdRemainders(nums: List[Int], n: Int): List[(Int, Int)] = {
    var res: List[(Int, Int)] = List()

    for (rem <- 0 to n) {
      res = res.appended((rem, nums.count(_ % n == rem)))
    }

    res
  }

  private abstract class Settlement(val population: Int, val area: Double)

  private case class Village(override val population: Int, override val area: Double, dense: Boolean)
    extends Settlement(population, area)

  private case class Town(override val population: Int, override val area: Double)
    extends Settlement(population, area)

  private case class City(override val population: Int, override val area: Double, val pool: Boolean)
    extends Settlement(population, area)

  private def filterSettlements(xs: List[Settlement]): List[Settlement] =
    xs.filter {
      case Village(_, _, dense) => dense
      case City(pop, _, pool) => pop > 150_000 && pool
    }
}
