package projecteuler
//Special subset sums: optimum
object Problem103 {
  import Problem105._
  def main(args: Array[ String]): Unit = {
    val r = (15 to 60).toArray.combinations(7).filter(test).minBy(_.sum)
    println(r.toList)
    println(r.mkString)
  }
}