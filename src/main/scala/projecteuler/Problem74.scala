package projecteuler

object Problem74 {
  def main(args: Array[String]): Unit = {
    def factorial(n: Int):Int = if (n == 0) 1 else n * factorial(n-1)
    val dfs = (0 to 9).zip((0 to 9).map(factorial)).toMap
    def sumFac(n: Int) = n.toString.map(i => dfs(i - '0')).sum
    def nonRepLen(n: Int) = {
      import scala.collection.mutable
      val seen = mutable.Set(n)
      var sumF = sumFac(n)
      while(!seen.contains(sumF)) {
        seen += sumF
        sumF = sumFac(sumF)
      }
      seen.size
    }
    val result = (1 to 1000000).foldLeft(0){(acc, n) =>
      if (nonRepLen(n) == 60) acc + 1 else acc
    }
    println(result)
  }
}