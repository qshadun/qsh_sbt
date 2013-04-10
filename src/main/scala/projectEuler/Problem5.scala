package projectEuler
import scala.collection.mutable.Map
/**
 * Smallest multiple
Problem 5
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
 */
object Problem5 {

  val primes = List(2,3,5,7,11,13,17,19)
  
  def findFactors(n: Int): List[Int] = {
    if (primes.contains(n)) List(n)
    else {
      val aFactor = primes.find(n % _ == 0).get
      val remain = n / aFactor
      aFactor :: findFactors(remain)
    } 
  }
  def groupFactors(fs: List[Int]): Map[Int, Int] = {
    val result = Map.empty[Int, Int]
    fs.distinct.foreach{ x =>
      result(x) = fs.filter(_ == x).size 
    }
    result
  }
  
  def gcd(x: Long, y: Long): Long = {
    if (x % y == 0) y
    else gcd(y, x % y)
  }
  
  def main(args: Array[String]): Unit = {
    //Brute force
//    def isSuitable(n: Long) = (2 to 20).forall(n % _ == 0)
//    val start = 2520L* 11 * 13 * 17 * 19
//    def candidates = Stream.iterate(2520L* 11 * 13 * 17 * 19){_ + 1}
//    println(candidates.dropWhile(!isSuitable(_)).head)
    
    // Find all factors
//    val allFactors = (2 to 20).map(findFactors).map(groupFactors) 
//    println(allFactors)
//    val allNeededFactors = allFactors.reduceLeft {(acc, m) =>
//      m.keys.foreach {x =>
//        if (acc.contains(x)) acc(x) = Math.max(acc(x), m(x))
//        else acc(x) = m(x)
//      }
//      acc
//    }
//    println(allNeededFactors)
//    val result = allNeededFactors.keys.foldLeft(BigInt(1)) {(acc, x) =>
//      acc * Math.pow(x, allNeededFactors(x)).toInt
//    }
//    println(result)
    
    // multiply 1~20 and devide gcds on the way 
    println((2L to 20L).reduce((x, y) => x * y / gcd(x, y)))
  }

}