object H06 {
  def main(args: Array[String]): Unit = {
    println(squareOrTenX(List(1, 2, 3)))
    println(squareOrTenX(List(1, 2, 3, 4)))
    println(onlyUppercase("HyeEeLLO WORLDt"))
    println(avgLength(List("hello", "world", "yeet")))
    println(filterAndFlip(List(1234, 12341, 6346, 141)))
    println(stringzz(List("sOME sTRING", "abc", "abba", "yeEeEt")))
  }

  /**
   * If the list length is even, square every element, otherwise multiply with 10. Use `map`.
   */
  def squareOrTenX(xs: List[Int]): List[Int] =
    if (xs.length % 2 == 0) xs.map(x => x * x)
    else xs.map(_*10)

  /**
   * Remove all uppercase letters, then using `map` convert the rest to uppercase.
   */
  def onlyUppercase(xs: String): String = xs.filter(_.isLower).map(_.toUpper)

  /**
   * Calculate the average length of a list of strings.
   */
  def avgLength(xs: List[String]): Float = xs.map(_.length).sum / xs.length.toFloat

  /**
   * Filter only numbers where the first and last digits are the same, then flip the numbers.
   */
  def filterAndFlip(xs: List[Int]): List[Int] = xs.filter(x => firstDigit(x) == lastDigit(x)).map(reverse)

  def firstDigit(n: Int): Int = n / Math.pow(10, Math.log10(n).toInt).toInt

  def lastDigit(n: Int): Int = n % 10

  def reverse(n: Int): Int = {
    var t = n
    var reverse = 0
    while (t != 0) {
      reverse = reverse * 10 + (t % 10)
      t /= 10
    }
    reverse
  }

  /**
   * Remove all strings with more than 4 uppercase letters.
   * Then for each string append to it the same string but reversed.
   * Then concatenate all the strings into one.
   */
  def stringzz(xs: List[String]): String = xs.filter(x => x.count(_.isUpper) <= 4).map(x => x ++ x.reverse).reduce(_ ++ _)
}
