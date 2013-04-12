package projecteuler
/**
 * Special Pythagorean triplet
 * Problem 9
 * A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,
 *
 * a2 + b2 = c2
 * For example, 32 + 42 = 9 + 16 = 25 = 52.
 *
 * There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 * Find the product abc.
 */
object Problem9 {

  def main(args: Array[String]): Unit = {
    val r = for (
      x <- 1 to 333;
      y <- x to (1000 - 2 * x);
      z = 1000 - x - y;
      if (x * x + y * y == z * z)
    ) yield (x, y, z, x * y * z)
    println(r.toList)
  }

}