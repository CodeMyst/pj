object V05 {
  def main(args: Array[String]): Unit = {
    println(charType('a'))

    println(multiplyWith(2, Array(1, 2, 3, 4, 5)))

    println(squareAll(Array(1, 2, 3, 4, 5)).mkString(", "))

    repeatChar('a', 10)

    // print("unesi a: ")
    // val a = scala.io.StdIn.readInt()

    // print("unesi b: ")
    // val b = scala.io.StdIn.readInt()

    // print("unesi op (+, -, *, /): ")
    // val op = scala.io.StdIn.readChar()

    // println(calculator(a, b, op))

    println(calculator(2, 3, '+'))

    println(isNumeric(3.1415926))
    println(isNumeric(":)"))

    println(tuplez((2, 3), 5))

    println(flatten(List(1, 2, 2, 2, 3, 3, 4, 5, 5, 5)))
  }

  /**
    * Checks the type of a char.
    */
  def charType(c: Char): String = {
    if (c >= 'a' && c <= 'z')
      "lowercase"
    else if (c >= 'A' && c <= 'Z')
      "uppercase"
    else
      "not a letter"
  }

  /**
    * Multiply the first number with all numbers from the array.
    */
  def multiplyWith(a: Int, b: Array[Int]): Int = {
    var res = a
    b.foreach(res *= _)

    res
  }

  /**
    * Square all elements of the array using yield. 
    */
  def squareAll(a: Array[Int]): Array[Int] = {
    for (x <- a) yield x * x
  }

  /**
    *  Print out the char `c` `n` times.
    */
  def repeatChar(c: Char, n: Int): Unit = {
    1 to n foreach {_ => print(c)}
    println()
  }

  /**
    * Simple calculator using match cases. 
    */
  def calculator(a: Int, b: Int, op: Char): Int = op match {
    case '+' => a + b
    case '-' => a - b
    case '*' => a * b
    case '/' => a / b
    case _   => 0
  }

  /**
    * Checks if the provided var is a numeric type.
    */
  def isNumeric(a: Any): Boolean = a match {
    case _: Int    => true
    case _: Short  => true
    case _: Long   => true
    case _: Float  => true
    case _: Double => true
    case _        => false
  }

  /**
    * Create a new tuple, first element multiplied by `n`, and second element added to `n`. 
    */
  def tuplez(a: (Int, Int), n: Int): (Int, Int) = (a._1 * n, a._2 + n)

  /**
    * Flattens a list. 
    */
  def flatten(a: List[Int]): List[Int] = a match {
    case Nil => Nil
    case x :: y :: xs => if (x == y) flatten(x :: xs) else List(x) ++ flatten(y :: xs)
    case x => x
  }
}
