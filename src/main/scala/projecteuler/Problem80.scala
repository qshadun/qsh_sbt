package projecteuler
/**
Square root digital expansion
Problem 80
It is well known that if the square root of a natural number is not an integer, then it is irrational. 
The decimal expansion of such square roots is infinite without any repeating pattern at all.

The square root of two is 1.41421356237309504880..., and the digital sum of the first one hundred decimal digits is 475.

For the first one hundred natural numbers, find the total of the digital sums of the first one hundred decimal digits for all the irrational square roots.
*/
object Problem80{
  def main(args: Array[String]) = {
    val mc = new java.math.MathContext(100, java.math.RoundingMode.DOWN)
    def toDecimal(n: BigInt, d: BigInt): String = (BigDecimal(n, mc) / BigDecimal(d, mc)).toString
    def toDecimalTuple = (toDecimal _).tupled
    
    def findRep(n: Int) = {
      val root = math.sqrt(n).toInt
      def findR(sofar: List[Int], seen: List[(Int,Int)]): List[Int] = {
        val remain = seen.head._1
        val d = seen.head._2
        val nd = (n - remain * remain) / d
        val nr = root - (remain + root) % nd
        if (seen.size > 1 && (nr, nd) == seen.init.last) sofar.reverse else findR((remain + root)/nd :: sofar, (nr, nd) :: seen)
      }
      findR(Nil, List((root, 1)))
    }
    def conv(b0: Int, as: Seq[Int], bs: Seq[Int]): Stream[(BigInt, BigInt)] = {
      def loop(x0:(BigInt, BigInt), x1:(BigInt, BigInt), n: Int): Stream[(BigInt, BigInt)] = {
        val xn = (bs(n) * x1._1 + as(n) * x0._1, bs(n) * x1._2 + as(n) * x0._2)
        xn #:: loop(x1, xn, n + 1)
      }
      val x0 = (BigInt(b0), BigInt(1))
      val x1 = (bs.head * BigInt(b0) + as.head, BigInt(bs.head))
      x0 #:: x1 #:: loop(x0, x1, 1)
    }
    def convSqrt(n: Int): Stream[(BigInt, BigInt)] = 
      conv(math.sqrt(n).toInt, Stream.continually(1), Stream.continually(findRep(n)).flatten)
    def sqrtDecimal(n: Int) = {
      val conv = convSqrt(n)
      var count = 0
      var x1 = conv(count)
      var r1 = toDecimalTuple(x1)
      var x2 = conv(count + 1)
      var r2 = toDecimalTuple(x2)
      while(r1 != r2) {
        count = count + 1
        x1 = x2
        r1 = r2
        x2 = conv(count + 1)
        r2 = toDecimalTuple(x2)
      }
      r1.filterNot(_ == '.')
    }
    val result = (1 to 100).filterNot((1 to 10).map(x => x * x).contains(_)).map{n =>
      sqrtDecimal(n).map(_ - '0').sum
    }.sum
    println(result)
  }
}
