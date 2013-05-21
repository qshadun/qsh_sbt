package projecteuler
import scala.collection.mutable.ArrayBuffer
//http://en.wikipedia.org/wiki/Euler's_totient_function
object Problem69 {
  def main(args: Array[String]) = {
    def sieve = Sieve(1000000)
    def isPrime(n: Int) = if (n < 1000000) sieve.isPrime(n) else (2 to math.sqrt(n).toInt).forall(n % _ != 0)
    val primes = sieve.getPrimes
    def primeFactors(n: Int) = {
      var pfs = ArrayBuffer.empty[Int]
      var i = 2
      var remain = n
      while(remain != 1 && i < n) {
        if (remain % i == 0) {
          pfs.append(i)
          while(remain % i == 0) remain = remain / i
        }
        i = i + 1
      }
      pfs.toList
    }
    
    def phi(n: Int): Int = 
      if (n == 1) 1
      else 
        if (isPrime(n)) n -1
        else {
          val fs = primeFactors(n)
          val norm = fs.foldLeft(BigInt(1)){case (acc, x) => acc * (x - 1)}
          val denorm = fs.foldLeft(BigInt(1))(_ * _)
          (n * norm / denorm).toInt
        }
    def subCalc(ls: Seq[Int]) = 
      ls.map(n => (n, phi(n))).filter{case (n, p) => n.toString.sorted == p.toString.sorted}
    val ab = ArrayBuffer.empty[(Int, Int)]
    for (i <- 1 to 1000) {
      val lb = if (i == 1) 2 else (i - 1) * 10000 
      val ub = i * 10000
      val sub = subCalc(lb until ub)
      if (!sub.isEmpty) {
        val min = sub.minBy{case (n, p) => n.toDouble / p}
        ab.append(min)
        println(min)
      }
    }
    println("final result: " + ab.minBy{case (n, p) => n.toDouble / p})
//    println(primes.scanLeft((1,1))((acc, x)=> (acc._1 * x, x)).takeWhile(_._1 <= 1000000).last)
//   println((3 to 1000000).foldLeft((2, 2d)){(acc,n) => 
//     val r = (n.toDouble / phi(n))
//     if (r > acc._2) (n, r) else acc
//   })
  }
}