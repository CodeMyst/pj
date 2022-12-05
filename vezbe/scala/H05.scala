import scala.language.postfixOps

object H05 {
  def main(args: Array[String]): Unit = {
    println(countEmpty(Array("hello", "", "world", "", "")))
    println(longerThan(Array("hello", "", "world", "", ""), 3))
    println(allPairs(Array(1, 2, 3, 4, 5)).mkString(", "))
    println(avgWithinRange(allPairs(Array(1, 2, 3, 4, 5)), 4, 5).mkString(", "))

    println(filterPrimes((1 to 20).toArray).mkString(", "))
    println(quickSort(Array(5, 7, 5, 2, 6, 4, 54, 76, 12, 3, 54)).mkString(", "))
  }

  /**
    * Count number of empty elements. 
    */
  def countEmpty(elems: Array[String]): Int = {
    elems.count(x => x == "")
  }

  /**
    * Count number of elements longer than `n`. 
    */
  def longerThan(elems: Array[String], n: Int): Int = {
    elems.count(x => x.length() > n)
  }

  /**
    * Return all pairs of an array. 
    */
  def allPairs(elems: Array[Int]): Array[Tuple2[Int, Int]] = {
    for (x <- elems; y <- elems) yield (x, y)
  }

  /**
    * Returns all pairs whose average is between the specified range. 
    */
  def avgWithinRange(pairs: Array[Tuple2[Int, Int]], lower: Int, higher: Int): Array[Tuple2[Int, Int]] = {
    pairs.filter(p => pairAvg(p) >= lower && pairAvg(p) <= higher)
  }

  def pairAvg(pair: Tuple2[Int, Int]): Float = {
    (pair._1 + pair._2).toFloat / 2f
  }

  /**
    * Scalar sum of two arrays.
    */
  def scalarProduct(a: Array[Int], b: Array[Int]): Array[Int] = {
    Array(0)
  }

  /**
    * How many items less than `n`. 
    */
  def lessThan(elems: Array[Int], n: Int): Int = {
    elems.count(e => e < n)
  }

  /**
    * Return all elements which are prime.
    */
  def filterPrimes(elems: Array[Int]): Array[Int] = {
    elems.filter(isPrime)
  }

  def isPrime(n: Int): Boolean = ! ((2 until n - 1) exists (n % _ == 0))

  def quickSort(elems: Array[Int]): Array[Int] = {
    if (elems.length <= 1) elems
    else {
        val pivot = elems(elems.length / 2)
        Array.concat(
            quickSort(elems filter (pivot >)),
            elems filter (pivot ==),
            quickSort(elems filter (pivot <))
        )
    }
  }
}
