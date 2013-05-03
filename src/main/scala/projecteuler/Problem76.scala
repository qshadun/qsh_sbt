package projecteuler
/**
http://mathworld.wolfram.com/PartitionFunctionP.html, item 11
P(0) = 1 and P(<0) = 0.
*/
object Problem76{
  def main(args: Array[String]) = {
    def summation(n: Int): Int = {
      def recur(i: Int, bound: Int, num: Int): Int = num match {
        case 1 => 1
        case _ =>
          (math.min(i-num+1, bound) to math.ceil(i.toDouble/num).toInt by -1).map{x =>
            recur(i - x, x, num -1)
          }.sum
      }
      (2 to n).map(recur(n, n, _)).sum
    }
    //println(summation(100))
    val cache = Array.fill(101)(0)
    cache(0) = 1
    def p(n: Int): Int = {
      if (n < 0) 0
      else if (cache(n) > 0) cache(n)
      else {
        cache(n) = (1 to n).map{k =>
          val sign = if (k % 2 == 0) -1 else 1
          sign * (p(n - k*(3*k-1)/2) + p(n - k*(3*k+1)/2))
        }.sum
        cache(n)
      }
    }
    println(p(100) - 1)
  }
}