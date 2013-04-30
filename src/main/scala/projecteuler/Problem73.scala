package projecteuler
object Problem73 {
  def main(args: Array[String]): Unit = {
    def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
    val result = (4 to 12000).foldLeft(0){(acc, d) =>
        acc + (1 to d).dropWhile(_ <= d/3).takeWhile(_ <= d/2).filter(gcd(_, d) == 1).size 
      }
    println(result)
  }
}