package projecteuler
import scala.collection.mutable.ArrayBuffer
/**
Just to formalise the great insights (and help anyone unfamiliar with phi(n))...

Given n=p1e1p2e2...p1e1, phi(n)=n(1-1/p1)(1-1/p2)...(1-1/pk).

Now for n/phi(n) to be minimised, phi(n) must be as close to n as possible; that is, we want to maximise phi(n).

When evaluating phi(n), we note that each time we multiply by (1-1/pi), it gets smaller, so we need to minimise 
the number of distinct prime factors in n.

As ookk explained, if n were prime, it would end in 1,3,7, or 9, and subtracting 1 only changes the 
last digit (to 0,2,6, or 8), so it could not be a permutation.

Hence n=p1*p2 and we only need to search through a list of known prime pairs.

In addition, phi(p1*p2)=p1*p2*(1-1/p1)(1-1/p2)=(p1-1)(p2-1), so we can compute phi(n) more efficiently.

A slick implementation of all these ideas will allow you to find the solution in a few seconds. 
As ookk also explained, we would expect n to be as close to ten million as possible, 
so the pair of prime factors would be around sqrt(10000000); remember we wish to minimise (1-1/pi), 
so pi must be as large as possible; my minimum prime was 1009.
 */
object Problem70 {
  def main(args: Array[String]) = {
  val bound = 10000000
	val sieve = Sieve(1000000)
	val primes = sieve.getPrimes.dropWhile(_ < 1009).takeWhile(_ <= 2 * math.sqrt(bound).toInt)
	val result = primes.map{x =>
	  primes.dropWhile(_ <= x).takeWhile(_ * x <= bound).filter{y =>
	    val n = x * y
	    val phi = (x - 1) * (y - 1)
	    n.toString.sorted == phi.toString.sorted
	  }.map(y => (x * y, (x - 1) * (y - 1), x, y))}.flatten.minBy{case (n, p, x, y) => n.toDouble / p}
	println(result)
  }
//  def main(args: Array[String]) = {
//    def sieve = Sieve(1000000)
//    def isPrime(n: Int) = if (n < 1000000) sieve.isPrime(n) else (2 to math.sqrt(n).toInt).forall(n % _ != 0)
//    val primes = sieve.getPrimes
//    
//    def primeFactors(n: Int) = {
//      var pfs = ArrayBuffer.empty[Int]
//      var i = 2
//      var remain = n
//      while(remain != 1 && i < n) {
//        if (remain % i == 0) {
//          pfs.append(i)
//          while(remain % i == 0) remain = remain / i
//        }
//        i = i + 1
//      }
//      pfs.toList
//    }
//    def phi(n: Int): Int = 
//      if (n == 1) 1
//      else 
//        if (isPrime(n)) n -1
//        else {
//          val fs = primeFactors(n)
//          val norm = fs.foldLeft(BigInt(1)){case (acc, x) => acc * (x - 1)}
//          val denorm = fs.foldLeft(BigInt(1))(_ * _)
//          (n * norm / denorm).toInt
//        }
//    def subCalc(ls: Seq[Int]) = 
//      ls.map(n => (n, phi(n))).filter{case (n, p) => n.toString.sorted == p.toString.sorted}
//    val ab = ArrayBuffer.empty[(Int, Int)]
//    for (i <- 1 to 1000) {
//      val lb = if (i == 1) 2 else (i - 1) * 10000 
//      val ub = i * 10000
//      val sub = subCalc(lb until ub)
//      if (!sub.isEmpty) {
//        val min = sub.minBy{case (n, p) => n.toDouble / p}
//        ab.append(min)
//        println(min)
//      }
//    }
//    println("final result: " + ab.minBy{case (n, p) => n.toDouble / p})
    
//  }
}