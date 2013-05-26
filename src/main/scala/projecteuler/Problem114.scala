package projecteuler
//Counting block combinations I
/**
The relationship with A005252 comes from the second definition there: "number of n-bit sequences that avoid both 010 and 0110", 
which is pretty much what we are looking for with 1=red, 0=black. Add an extra black at each end to make them identical.

I finally did the maths, and found that it is a very simple recurrence: a(n) = 2a(n-1) - a(n-2) + a(n-4) with initial values [1, 1, 1, 2]. 
 */
object Problem114 {
  import scala.collection.mutable
  val cache = mutable.Map.empty[Int, Long]
  cache(0) = 0
  cache(1) = 1
  cache(2) = 1
  cache(3) = 2
  def solve(size: Int): Long =
    if (cache.contains(size)) cache(size)
    else {
        val red = (3 to size).map { i =>
          if (size - i > 3) solve(size - i - 1)
          else 1
        }.sum
        val black = solve(size - 1)
        val result = red + black
        cache(size) = result
        result
    }
  def main(args: Array[String]): Unit = {
    println(solve(50))
  }
}