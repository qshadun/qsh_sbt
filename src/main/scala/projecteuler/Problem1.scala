package projecteuler

/**
 * Multiples of 3 and 5
 * If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
 * Find the sum of all the multiples of 3 or 5 below 1000.
 */
object Problem1 {

  // Wrong, this will calculate all numbers only dividable by 3 or 5
  def findAllNumbers(max: Int): List[Int] = {
    val start = List(3, 5)
    def f(l: List[Int]): Stream[List[Int]] = {
      val n = for (x <- l) yield List(x * 3, x * 5)
      l #:: f(n.flatten)
    }
    f(start).takeWhile(ls => ls.exists(_ <= max)).toList.flatten.distinct.sortWith(_ < _).takeWhile(_ <= max)
  }
  
  def findAllNumbers1(max: Int): List[Int] = {
    (1 until max).filter(x => x % 3 == 0 || x % 5 == 0).toList
  }
  
  def sumMultipleoOf3And5(max: Int): Int = {
    List.range(3, max, 3).sum + List.range(5, max, 5).sum - List.range(15, max, 15).sum
  }
  def main(args: Array[String]): Unit = {
    val allLessThan1000 = findAllNumbers1(1000) 
    println(allLessThan1000)
    println(allLessThan1000.sum)
    println(sumMultipleoOf3And5(1000))
  }

}