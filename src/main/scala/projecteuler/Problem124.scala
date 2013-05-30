package projecteuler
//Ordered radicals
object Problem124 {
  def main(args: Array[String]): Unit = {
    val limit = 100000 
    val sieve = Sieve(limit + 1)
    def isPrime = sieve.isPrime _
    val primes = sieve.getPrimes
    def getPrimeFactors(n: Int) = {
      def removeFactor(x: Int, f: Int): Int = if (x % f == 0) removeFactor(x / f, f) else x 
      def recur(remain: Int, sofar: List[Int], pm: Seq[Int]): List[Int] =
        if (remain == 1) sofar
        else if (isPrime(remain)) remain +: sofar
        else {
          val t = pm.dropWhile(remain % _ != 0)
          recur(removeFactor(remain, t.head), t.head +: sofar, t.tail)
        }
      if (isPrime(n)) List(n)
      else recur(n, Nil, primes.takeWhile(_ <= n))
    }
    def rad(n: Int) = getPrimeFactors(n).product
    val result = (1 to limit).map(i => (i, rad(i))).sortWith{case((n1, r1), (n2, r2)) => if (r1 == r2) n1 - n2 < 0 else r1 - r2 < 0}
    println(result(9999))
  }
}