package projecteuler
//Efficient exponentiation
object Problem122 {
  import scala.collection.mutable
  val cache = mutable.Map(1 -> List(List.empty[Int]))
  (1 to 7).foreach { i =>
    val n = math.pow(2, i).toInt
    cache(n) = List((1 to i).map(math.pow(2, _).toInt).toList)
  }
  def solve(n: Int): List[List[Int]] =
    if (cache.contains(n)) cache(n)
    else {
      val t = (1 to n / 2).map { i =>
        val can = for (
          ls1 <- solve(i);
          ls2 <- solve(n - i)
        ) yield (ls1 ++ ls2).distinct
        val min = can.minBy(_.size).size
        can.filter(_.size == min)
      }.flatten
      val min = t.minBy(_.size).size
      val result = t.filter(_.size == min).toList.map(n +: _)
      cache(n) = result
      result
    }
  def m(n: Int) = solve(n).head.size
  def main(args: Array[String]): Unit = {
    val r = (1 to 200).map(m).sum
    println(r)
  }
}