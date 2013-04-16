package projecteuler

object Problem34 {
/**
Digit factorials
Problem 34
145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial of their digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included.
 */
  
  def fact(n: Int):Int = if ( n == 0) 1 else n * fact(n - 1)
  val digitFacts = (0 to 9).map(x => (x, fact(x))).toList.toMap 
  val upLimit = fact(9) * 7 //2540160, 7 digits
  def digitFact(n: Int) = n.toString.toCharArray.map(_ - '0').foldLeft(0){(acc, x) =>
    acc + digitFacts(x)
  }
  def main(args: Array[String]): Unit = {
    val all = (10 to 2540160).filter(x => x == digitFact(x))
    println(all)
    println(all.sum)
  }

}
