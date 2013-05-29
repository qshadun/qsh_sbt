package projecteuler
//Prime square remainders
object Problem123 {
  def main(args: Array[String]) = {
    val sieve = Sieve(1000000)
    val primes = sieve.getPrimes.map(BigInt.apply)
    val limit = BigInt(10).pow(10)
    val r = primes.zipWithIndex.find{case (p, i) => i % 2 == 0 && p * 2 * (i + 1) > limit}
    println(r)
  }
}