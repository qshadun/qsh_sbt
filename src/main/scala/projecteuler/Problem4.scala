package projecteuler

/**
 * Largest palindrome product
Problem 4
A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 99.

Find the largest palindrome made from the product of two 3-digit numbers.
 */
object Problem4 {

  def isPalindromic(n: Int) = {
    n.toString == n.toString.reverse.mkString
  }
  
   
   
  def main(args: Array[String]): Unit = {
      val v = for(x <- List.range(999, 900, -1); 
                  y <- List.range(x, 900, -1);
                  val p = x * y if isPalindromic(p)) yield (p, x, y)
      println(v.sortWith(_._1 > _._1).head)
      //println(v.dropWhile(!isPalindromic(_)).head)
  }

}