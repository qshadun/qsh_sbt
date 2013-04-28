package projecteuler
object Problem69 {
  def main(args: Array[String]) = {
    def sieve = Sieve(1000001)
    def isPrime = sieve.isPrime _
    val primes = sieve.getPrimes
    def factorials(n: Int) = {
      def recur(remain: Int, fs: Seq[Int]): List[Int] = {
        if (remain == 1) Nil
        else {
          var r = remain
          if (r % fs.head == 0) {
            while (r % fs.head == 0) r = r / fs.head
            if (isPrime(r)) List(fs.head, r)
            else fs.head :: recur(r, fs.tail)
          }
          else recur(r, fs.tail)
        }
      }
      recur(n, primes.takeWhile(_ <= n / 2))
    }
    println(factorials(1000000))
    def phi(n: Int): Int = 
      if (isPrime(n)) n -1
      else 
        factorials(n).foldLeft(Rational(n, 1)){(acc, x) =>
          acc * Rational(x - 1, x)
        }.normalize.n.toInt
    
    println(primes.scanLeft((1,1))((acc, x)=> (acc._1 * x, x)).takeWhile(_._1 <= 1000000).last)
//   println((3 to 1000000).foldLeft((2, 2d)){(acc,n) => 
//     val r = (n.toDouble / phi(n))
//     if (r > acc._2) (n, r) else acc
//   })
  }
}