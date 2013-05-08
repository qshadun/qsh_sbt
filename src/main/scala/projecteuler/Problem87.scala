package projecteuler
object Problem87 {
  def main(args: Array[String]): Unit = {
    val bound = 50000000
    val ar = Array.fill(bound + 1)(0)
    val sieve = Sieve(math.sqrt(bound).toInt)
    val sqPrimes =  sieve.getPrimes
    val cbPrimes = sqPrimes.takeWhile(_ <= math.cbrt(bound).toInt)
    val quaPrimes = cbPrimes.takeWhile(_ <= math.sqrt(math.sqrt(bound)).toInt)
    for (i <- sqPrimes; j <- cbPrimes; k <- quaPrimes) {
      val r = i * i + j * j * j + k * k * k * k
      if (r <= bound) ar(r) = 1
    }
    println(ar.sum)
  }
}