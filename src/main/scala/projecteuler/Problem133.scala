package projecteuler
import Utils._
import MultiplicativeOrder._
object Problem133 {
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  def bruteForce(n: Int) = {
    var x = 1
    while (BigInt(10).modPow(x, 9 * n) != 1) x = x + 1
    x
  }
  // Check if n only has prime factors 2 and 5
  def check(n: Int): Boolean =
    if (n == 1) true
    else if (n % 2 == 0) check(n / 2)
    else if (n % 5 == 0) check(n / 5)
    else false
  def main(args: Array[String]): Unit = {
    val sieve = Sieve(100000)
    val primes = sieve.getPrimes
    val result = primes.filterNot(x => gcd(x, 10) == 1 && check(multOrder(10, x)))
    println(result)
    println(result.map(_.toLong).sum)
  }
} 