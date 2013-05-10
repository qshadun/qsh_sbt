package projecteuler
// Square digit chains
object Problem92 {
  val cache = Array.fill(9 * 9 * 7 + 1)(0)
  cache(1) = 2
  cache(89) = 1
  def squareDigits(n: Int): Int = 
    if (n < 10) n * n else ((n % 10) * (n % 10)) + squareDigits(n / 10)
  def check(n: Int): Boolean = {
    def recur(x: Int, seen: List[Int]): Boolean = cache(x) match {
      case 1 =>
        seen.foreach(cache(_) = 1)
        true
      case 2 =>
        seen.foreach(cache(_) = 2)
        false
      case _ =>
        val sd = squareDigits(x) 
        recur(sd, sd :: seen)
    }
    val sd = squareDigits(n) 
    recur(sd, Nil)
  }
  def main(args: Array[String]): Unit = {
    println((1 to 10000000).filter(check).size)
  }
}