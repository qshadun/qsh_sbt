package projecteuler
/**
 * Prime pair sets
 * Problem 60
 * The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order the
 * result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes, 792,
 * represents the lowest sum for a set of four primes with this property.
 *
 * Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.
 */
object Problem60 {
  def main(args: Array[String]): Unit = {
    val sieve = new Sieve(1000000)
    def isPrime = sieve.isPrime _
    def isPrimeLarge(n: Int) = sieve.getPrimes.takeWhile(_ <= math.sqrt(n).toInt).forall(n % _ != 0)
    def check(ls: Seq[Int], n: Int) = {
      ls.map { x =>
        val n1 = x.toString + n.toString
        val n2 = n.toString + x.toString
        List(n1, n2)
      }.flatten.forall { x =>
        x.length < 7 && isPrime(x.toInt) ||
          isPrimeLarge(x.toInt)
      }
    }
    val searchSpace = sieve.getPrimes.takeWhile(_ < 10000)
    import scala.util.control.Breaks._
    breakable {
      for (
        x1 <- searchSpace;
        x2 <- searchSpace;
        if x2 > x1 && check(List(x1), x2);
        x3 <- searchSpace;
        if x3 > x2 && check(List(x1, x2), x3);
        x4 <- searchSpace;
        if x4 > x3 && check(List(x1, x2, x3), x4);
        x5 <- searchSpace;
        if x5 > x4 && check(List(x1, x2, x3, x4), x5)
      ) {
          val result = List(x1, x2, x3, x4, x5) 
          println(result + ", " + result.sum)
          break
      }
    }
  }
}
