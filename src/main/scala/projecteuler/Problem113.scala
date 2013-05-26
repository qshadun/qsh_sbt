package projecteuler
//Non-bouncy numbers
object Problem113 {
  import scala.collection.mutable
  val increaseCache = mutable.Map.empty[(Int, Int), Long]
  val decreaseCache = mutable.Map.empty[(Int, Int), Long]
  def countIncrease(numOfDigits: Int): Long = {
    def recur(n: Int, start: Int): Long =
      if (increaseCache.contains(n, start)) increaseCache((n, start))
      else {
        val r = if (n == 1) 9 - start + 1
        else {
          start to 9 map { recur(n - 1, _) } sum
        }
        increaseCache((n, start)) = r
        r
      }
    if (numOfDigits == 1) 9
    else 1 to 9 map { recur(numOfDigits - 1, _) } sum
  }
  def countDecrease(numOfDigits: Int): Long = {
    def recur(n: Int, start: Int): Long =
      if (decreaseCache.contains(n, start)) decreaseCache((n, start))
      else {
        val r = if (n == 1) start + 1
        else {
          0 to start map { recur(n - 1, _) } sum
        }
        decreaseCache((n, start)) = r
        r
      }
    if (numOfDigits == 1) 9
    else 1 to 9 map { recur(numOfDigits - 1, _) } sum
  }
  def count(numOfDigits: Int) = 1 to numOfDigits map { i => countIncrease(i) + countDecrease(i) - 9 } sum
  /**
   * I thought of this problem in terms of the number of ways to increase, decrease, or keep each successive digit the same.
   * Across an increasing number, you can only increase the digit 8 times (since we don't have a leading 0).
   * Across a decreasing number you can decrease up to 9 times.
   * Additionally, for each digit in the number, you have a "keep the same" option.
   * The answer is then just the number of ways you can increase or keep the same plus the number of ways you can decrease or keep the same.
   * You have to be careful here, since keeping the entire number the same will be counted twice.
   */
  def choose(n: Int, r: Int): Long =
    if (r == 0) 1L
    else {
      val k = if (r <= n / 2) r else n - r
      (n to n - k + 1 by -1 map { _.toLong } product) / (1 to k map { _.toLong } product)
    }

  def countByChoose(n: Int) =
    (1 to n).foldLeft(0L) { (acc, i) =>
      acc + choose(i + 8, 8) + choose(i + 9, 9) - 10
    }

  def main(args: Array[String]): Unit = {
    println(countByChoose(100))
  }
}