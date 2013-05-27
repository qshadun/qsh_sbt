package projecteuler
//Red, green, and blue tiles
object Problem117 {
  def f(size: Int, blockSizes: List[Int]): Long = {
    import scala.collection.mutable
    val cache = mutable.Map.empty[Int, Long]
    val min = blockSizes.min
    0 until min foreach { i => cache(i) = 0 }
    cache(min) = 1
    def recur(size: Int): Long =
      if (cache.contains(size)) cache(size)
      else {
        val colored = blockSizes.map { i =>
          if (size >= i) recur(size - i) + 1
          else 0
        }.sum
        val black = recur(size - 1)
        val result = colored + black
        cache(size) = result
        result
      }
    recur(size) + 1 //all black
  }
  def main(args: Array[String]): Unit = {
    println(f(50, List(2, 3, 4)))
  }
}