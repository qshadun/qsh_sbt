package projecteuler
//Large repunit factors
object Problem132 {
  def main(args: Array[String]): Unit = {
    val sieve = Sieve(1000000)
    val primes = sieve.getPrimes.drop(3) // gcd(2, 10) != 1
    val target = BigInt(10).pow(9)
    val limit = 40
    val result = primes.filter(x => BigInt(10).modPow(target, 9 * x) == 1).take(limit)
    println(result.toList)
    println(result.sum)
  }
}