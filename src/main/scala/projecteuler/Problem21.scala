package projecteuler
/**
Amicable numbers
Problem 21
Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
If d(a) = b and d(b) = a, where a != b, then a and b are an amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. 
The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.
 */
object Problem21 {
  
  def divisibles(n: Int) = (1 to Math.sqrt(n).toInt).foldLeft(List.empty[Int]){(acc, x) =>
    if (n % x == 0) 
      if (x * x == n) x :: acc
      else x :: (n / x) :: acc
    else acc
  }
  def d(n: Int) = divisibles(n).sum - n
  
  def isAmicable(n: Int) = {
    val dn = d(n)
    dn != n && d(dn) == n
  }
  
  def main(args: Array[String]): Unit = {
    println((1 until 10000).filter(isAmicable).sum)
  }

}