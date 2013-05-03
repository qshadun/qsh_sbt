package projecteuler
/**
http://mathworld.wolfram.com/PartitionFunctionP.html, item 11
P(0) = 1 and P(<0) = 0.
*/
object Problem78{
  def main(args: Array[String]) = {
    val bound = 100000
    val cache = Array.fill(bound + 1)(BigInt(0))
    cache(0) = BigInt(1)
    def p(n: Int): BigInt = {
      if (n < 0) BigInt(0)
      else if (cache(n) > 0) cache(n)
      else {
        cache(n) = (1 to n).map{k =>
          val sign = if (k % 2 == 0) -1 else 1
          val n1 = n.toLong - k.toLong * (3 * k.toLong - 1) / 2
          val n1Int = if (n1 >= 0) n1.toInt else -1
          val n2 = n.toLong - k.toLong * (3 * k.toLong + 1) / 2
          val n2Int = if (n2 >= 0) n2.toInt else -1
          sign * (p(n1Int) + p(n2Int))
        }.sum
        cache(n)
      }
    }
    val result = (1 to bound).find{n =>
      p(n)
      cache(n) % 1000000 == 0
    }
    println(result)
    println(cache(result.get))
  }
}