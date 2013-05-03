package projecteuler
object Problem77{
  def main(args: Array[String]) = {
    val sieve = Sieve(1000)
    val primes = sieve.getPrimes
    val isPrime = sieve.isPrime _
    val MIN = 5000
    val cache = Array.fill(1500)(Seq.empty[List[Int]])
    def primePartition(n: Int) = {
      val p = primes.takeWhile(_ < n).map {h =>
        cache(n-h).filter(_.head >= h).map(h :: _)
      }.flatten
      cache(n) = 
        if (isPrime(n)) p :+ List(n)
        else p
    }
    val result = (2 to 500).find{n =>
      primePartition(n)
      cache(n).size >= MIN
    }
    println(result)
  }
}