package projecteuler
//Disc game prize fund
object Problem121 {
  def solve(n: Int):(Long, Long) = {
    val redTimesToWin = n / 2  + 1  
    val norm = (1 to n - redTimesToWin).map{blue =>
      (1 to n).map(_.toLong).combinations(blue).map(_.product).sum
    }.sum + 1
    val denorm = (1 to n).map(_.toLong + 1).product
    (norm, denorm)
  }
  def main(args: Array[String]): Unit = {
    val r = solve(15)
    println(r)
    println(r._2 / r._1)
  }
}