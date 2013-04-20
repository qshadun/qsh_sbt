package projecteuler

case class Sieve(bound: Int) {
  private val sieve = Array.fill(bound)(0)
  (2 to math.sqrt(bound).toInt).foreach { x =>
    if (sieve(x) == 0)
      ((x + x) until bound by x).foreach { i =>
        sieve(i) = sieve(i) + 1
      }
  }
  def getPrimes = for( i <- 2 until bound; if sieve(i) == 0) yield i
  def isPrime(n: Int) = {
    require(n < bound && n > 0)
    sieve(n) == 0
  }
}