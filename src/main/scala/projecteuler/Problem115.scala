package projecteuler
//Counting block combinations II
object Problem115 {
  import scala.collection.mutable
  val cache = mutable.Map.empty[Int, Long]
  def f(size: Int, min: Int): Long =
    if (cache.contains(size)) cache(size)
    else {
        val red = (min to size).map { i =>
          if (size - i > min) f(size - i - 1, min)
          else 1
        }.sum
        val black = f(size - 1, min)
        val result = red + black
        cache(size) = result
        result
    }
  def main(args: Array[String]): Unit = {
    val min = 50
    1 to min foreach {i => cache(i) = 1}
    cache(min) = 2
    val result = Stream.from(1).find(i => f(i, min) > 1000000)
    println(result)
  }
}