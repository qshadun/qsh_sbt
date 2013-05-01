package projecteuler

object Problem74 {
  def main(args: Array[String]): Unit = {
    def factorial(n: Int): Int = if (n == 0) 1 else n * factorial(n - 1)
    val dfs = (0 to 9).zip((0 to 9).map(factorial)).toMap
    def sumFac(n: Int) = n.toString.map(i => dfs(i - '0')).sum
    import scala.collection.mutable
    val cache = Array.fill(6 * factorial(9) + 1)(0)
    def nonRepLen(n: Int) = {
      if (cache(n) != 0) cache(n)
      else {
        val seen = mutable.Map(n -> 0)
        var sumF = sumFac(n)
        var i = 1
        while (!seen.contains(sumF) && cache(sumF) == 0) {
          seen(sumF) = i
          sumF = sumFac(sumF)
          i = i + 1
        }
        if (seen.contains(sumF)) {
          cache(sumF) = seen.size - seen(sumF)
          seen.map {
            case (v, i) =>
              if (i < seen(sumF)) cache(v) = cache(sumF) + seen(sumF) - i
              else if (i > seen(sumF)) cache(v) = cache(sumF) + seen.size - i
          }
           seen.size + cache(sumF) - 1
        } else {
          seen.map{case(v, i) => cache(v) = seen.size - i + cache(sumF)}
          seen.size + cache(sumF)
        }
      }
    }
    val result = (1 to 1000000).filter(nonRepLen(_) == 60).size
    println(result)
  }
}