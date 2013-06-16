package projecteuler
//Prime cube partnership
/**
n^3 + p*n^2 = (n+m)^3
m = (sqrt(4p/m - 3) - 3) * n / 2 => n = 2 * m / (sqrt(4*p*m-3*m*m) - 3*m)
m < p / 3
 */
object Problem131 {
  def isPerfectSquare(n: Long) = {
    val root = math.sqrt(n).toLong
    root * root == n
  }
  /**
1) n^3 + n^2*p = n^2*(n + p), which, since p is prime can only be a perfect cube if both n and (n + p) are themselves perfect cubes; therefore p must be a difference of cubes.

2) Since a^3 - b^3 = (a-b)(a^2 + ab + b^2), only the difference of consecutive cubes can be prime -- i.e. a - b = 1.

3) So just check the (monotonically increasing) sequence of differences of consecutive cubes for primes.

(I believe there are 9289 of them below 1010)
   */
  
  def main(args: Array[String]): Unit = {
    val target = if (args.isEmpty) 1000000 else args(0).toInt
    val sieve = Sieve(target)
//    val primes = sieve.getPrimes
//    def solve(p: Int): Option[(Int, (Int, Int))] = {
//      val x = (1 to p/3)find{m => 
//        val t = 4L * p *m - 3L * m * m
//        isPerfectSquare(t) && {
//          val tt = math.sqrt(t).toLong
//          tt > 3 * m && 2L * m * m % (tt - 3 * m) == 0
//        }
//      }
//      x.map{m =>
//            val tt = math.sqrt( 4L * p *m - 3L * m * m).toLong
//            val n = 2L * m * m / (tt - 3 * m)
//            (p, (n.toInt, m))
//          }
//    }
//    val result = primes.map(solve).filter(_.isDefined)
//    println(result.toList)
//    println(result.size)
    val result = Stream.from(1).map(k => 3 * k * k + 3 * k + 1).takeWhile(_ < target).filter(sieve.isPrime(_))
    println(result.size)
  }
}