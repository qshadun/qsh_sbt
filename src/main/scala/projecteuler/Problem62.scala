package projecteuler
object Problem62 {
  def main(args: Array[String])  = {
    val cubs = Stream.from(1).map(x => BigInt(x) * x * x)
    def pow(a: Int, b: Int): BigInt = if (b == 0) BigInt(1) else a * pow(a, b-1)
    def find(n: Int):BigInt = {
      val m = cubs.dropWhile(_ < pow(10, n-1)).takeWhile(_ < pow(10, n)).toList.groupBy(_.toString.sorted).filter(_._2.size == 5)
      if (m.size > 0) m.toList.sortBy(_._2.head).head._2.head else find(n+1)
    }
    println(find(3))
  }
}