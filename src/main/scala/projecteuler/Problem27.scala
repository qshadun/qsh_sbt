package projecteuler
/**
Quadratic primes
Problem 27
Euler published the remarkable quadratic formula:

n^2 + n + 41

It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. However, when n = 40, 
402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41^2 + 41 + 41 is clearly divisible by 41.

Using computers, the incredible formula  n^2  79n + 1601 was discovered, which produces 80 primes for the consecutive values n = 0 to 79. 
The product of the coefficients, -79 and 1601, is -126479.

Considering quadratics of the form:

n^2 + an + b, where |a|  1000 and |b|  1000

where |n| is the modulus/absolute value of n
e.g. |11| = 11 and |-4| = 4
Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.
 */
object Problem27 {

  val primes = Stream.iterate(2L)(_ + 1).filter(x => (2L to Math.sqrt(x).toLong).forall(x % _ != 0))
  def isPrime(n: Long) = primes.dropWhile(_ < n).head == n 
  def numOfQuadratics(a: Int, b: Int) = Stream.from(0).takeWhile(x => isPrime(1L * x * x + a * x + b)).size
  def main(args: Array[String]): Unit = {
    val bCandidates = primes.takeWhile(_ < 1000).map(_.toInt).toList
    val all = for(
          a <- -999 to 999;
          b <- bCandidates
        ) yield((a, b, numOfQuadratics(a, b)))
    val r = all.reduceLeft{(acc, x) =>
      if (x._3 > acc._3) x else acc
    }
    println(r + ", a*b =" + r._1 * r._2)
//    println(primes.take(10).toList)
//    println(numOfQuadratics(1, 41))
//    println(numOfQuadratics(-79, 1601))
  }

}