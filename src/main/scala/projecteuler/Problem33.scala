package projecteuler
/**
 * Digit canceling fractions
 * Problem 33
 * The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may
 * incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.
 *
 * We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
 *
 * There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing
 * two digits in the numerator and denominator.
 *
 * If the product of these four fractions is given in its lowest common terms, find the value of the denominator.
 */
object Problem33 {
  case class Rational(n: Int, d: Int) {
    def this(x: (Int, Int)) = this(x._1, x._2)
    require(d != 0)
    def normalize = {
      val g = gcd(n, d)
      new Rational(n / g, d / g)
    }
  }
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  val digits = (1 to 9).map(_.toString).toList

  def main(args: Array[String]): Unit = {
    // ij/ki, not possible
//    val r1 = for (
//      i <- digits;
//      j <- digits;
//      k <- digits;
//      val ij = (i + j).toInt;
//      val ki = (k + i).toInt;
//      if ij < ki && Rational(ij, ki).normalize == Rational(j.toInt, k.toInt).normalize
//    ) yield (ij, ki)
    // ji/ik    
    val r2 = for (
      i <- digits;
      j <- digits;
      k <- digits;
      ji = (j + i).toInt;
      ik = (i + k).toInt;
      if ji < ik && Rational(ji, ik).normalize == Rational(j.toInt, k.toInt).normalize
    ) yield (ji, ik)

//    println(r1)
    println(r2)
    val result = new Rational(r2.reduceLeft{(acc, x) =>
      (acc._1 * x._1, acc._2 * x._2)
    }).normalize.d
     
    println(result)
  }

}