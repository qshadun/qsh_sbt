package projecteuler
//Red, green or blue tiles
object Problem116 {
  def f(size: Int, min: Int): Long = {
    import scala.collection.mutable
    val cache = mutable.Map.empty[Int, Long]
    0 until min foreach { i => cache(i) = 0 }
    cache(min) = 1
    def recur(size: Int): Long =
      if (cache.contains(size)) cache(size)
      else {
        val red = recur(size - min) + 1
        val black = recur(size - 1)
        val result = red + black
        cache(size) = result
        result
      }
    recur(size)
  }
  def solve(n: Int) = f(n, 2) + f(n, 3) + f(n, 4)
  def main(args: Array[String]): Unit = {
    println(solve(50))
  }
}