package projecteuler
/**
Combinatoric selections
Problem 53
There are exactly ten ways of selecting three from five, 12345:

123, 124, 125, 134, 135, 145, 234, 235, 245, and 345

In combinatorics, we use the notation, 5C3 = 10.

In general,

nCr = n!/(r! *(nr)!)
,where r  n, n! = n(n1)...321, and 0! = 1.
It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.

How many, not necessarily distinct, values of  nCr, for 1 <= n <= 100, are greater than one-million?
 */
object Problem53 {
  def main(args: Array[String]): Unit = {
    def factor(n: Int): BigInt = if (n == 0) 1 else n * factor(n - 1)
    def combinator(n: Int, r: Int):BigInt = (n - r + 1 to n).foldLeft(BigInt(1))(_ * _) / factor(r) 
    def findCombines(n: Int, bound: Int): Int = {
      def findR(r: Int): Int = 
        if (combinator(n, r) > bound) 
          if (r == n/2 && n % 2 == 0) 1 + findR(r-1) else 2 + findR(r - 1)
        else 0
      findR(n/2)
    }
    println((1 to 100).map(findCombines(_, 1000000)).sum)
   }
}