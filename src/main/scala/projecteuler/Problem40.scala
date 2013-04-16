package projecteuler
/**
 * Champernowne's constant
 * Problem 40
 * An irrational decimal fraction is created by concatenating the positive integers:
 *
 * 0.123456789101112131415161718192021...
 *
 * It can be seen that the 12th digit of the fractional part is 1.
 *
 * If dn represents the nth digit of the fractional part, find the value of the following expression.
 *
 * d1 * d10 * d100 * d1000 * d10000 * d100000 * d1000000
 */
object Problem40 {
  def main(args: Array[String]): Unit = {
    val c = {
      def loop(n: Int, i: Int): Stream[Int] =
        if (i < n.toString.length)
          (n.toString.charAt(i) - '0') #:: loop(n, i + 1)
        else
          loop(n + 1, 0)
      loop(1, 0)
    }
    println(c(0) * c(9) * c(99) * c(999) * c(9999) * c(99999) * c(999999))
  }

}