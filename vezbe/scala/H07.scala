object H07 {
  def main(args: Array[String]): Unit = {
    val savings = List(
      RSDSavings(2, termed = false),
      RSDSavings(3, termed = true),
      ForeignSavings(5, termed = true, eur = true)
    )

    println(rsdWithInterestRate(savings))

    println(termedAndEur(savings))

    println(avgInterestRate(savings))
  }

  private def rsdWithInterestRate(savings: List[Savings]): List[Savings] = savings.filter {
    _ match {
      case RSDSavings(rate, _) => rate > 2
      case _ => false
    }
  }

  private def termedAndEur(savings: List[Savings]): List[Savings] = savings.filter {
    _ match {
      case RSDSavings(_, termed) => termed
      case ForeignSavings(_, _, eur) => eur
    }
  }

  private def avgInterestRate(savings: List[Savings]): Double = {
    val filtered = savings.filter(_.termed)

    filtered.map(_.interestRate).sum / filtered.length.toDouble
  }
}

abstract class Savings(val interestRate: Double, val termed: Boolean)

case class RSDSavings(override val interestRate: Double, override val termed: Boolean)
  extends Savings(interestRate, termed)

case class ForeignSavings(override val interestRate: Double, override val termed: Boolean, eur: Boolean)
  extends Savings(interestRate, termed)
