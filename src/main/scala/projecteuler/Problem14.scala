package projecteuler
import scala.collection.mutable.ListBuffer
/**
Longest Collatz sequence
Problem 14
The following iterative sequence is defined for the set of positive integers:

n  n/2 (n is even)
n  3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13  40  20  10  5  16  8  4  2  1
It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
 */
object Problem14 {
  
  def main(args: Array[String]): Unit = {
    def collatzCount(n: Int) =  {
      var count = 1
      var x = n + 0L
      while (x != 1) {
        if (x % 2 == 0) x = x/2
        else x = 3*x + 1
        count = count + 1
      }
      count
    }
//    println(collatzCount(13))
//    var max = (1, 1)
//    var i = 2
//    while (i < 1000000) {
//      val c = collatzCount(i)
//      max = if (c > max._2) (i, collatzCount(i)) else max
////      if (i % 1000 == 0)
////        println(i + "," + max)
//      i = i + 1
//    }
//    println("doen: " + max)
    
    //It's slow to construct the whole list
    //only count the numbers is faster
    def collatz(n: Long): List[Long] =  n match  {
      case 1L => List(1L)
      case x if n % 2 == 0 => x :: collatz(x / 2)
      case _ => n :: collatz(3 * n + 1)
    }
//    println(collatz(13))
    val max = (2 until 1000000).foldLeft((1, 1)){(acc, x) =>
      val c = collatzCount(x)
      if (c > acc._2)  (x, c) else acc
    }
    println("doen: " + max)
  }

}