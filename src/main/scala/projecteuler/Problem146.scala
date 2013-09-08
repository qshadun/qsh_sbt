package projecteuler

object Problem146 {
  val sieve = Sieve(1000000)
  def isPrime(n: Long) = if (n < 1000000) sieve.isPrime(n.toInt) else BigInt(n).isProbablePrime(20)
  def solve(limit: Long) = {
    var sum = 0L
    var i = 10L
    while (i <= limit) {
      if (check(i)) sum = sum + i
      i = i + 10
    }
    sum
  }

  val nums = Seq(1, 3, 7, 9, 13, 27).map(_.toLong)
  val betweens = Seq(11, 17, 19, 21, 23).map(_.toLong)
  def check(n: Long) = {
    val sq = n * n
    nums.map(_ + sq).forall(isPrime) && betweens.map(_ + sq).forall(!isPrime(_))
  }
  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(solve(1000000L * 150))
    println(s"Time: ${System.currentTimeMillis() - start}ms")
  }
}