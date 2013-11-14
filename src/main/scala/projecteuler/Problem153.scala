package projecteuler

object Problem153 {
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  val limit = math.pow(10, 8).toInt
  def countNature = {
    var count = 0L
    var i = 1L
    while (i <= limit) {
      count = count + i * (limit / i)
      i = i + 1
    }
    count
  }
  def countComplex = {
    var count = 0L
    var x = 1
    while (x <= limit / 2) {
      val uBound = limit / x
      for (
        a <- 1 to math.sqrt(uBound - 1).toInt;
        b <- 1 to math.min(a, math.sqrt(uBound - a * a).toInt);
        if (gcd(a, b) == 1)
      ) {
        val times = uBound / (a * a + b * b)
        val c = if (a == b) 2L * a * x * times else (2 * a + 2 * b).toLong * x * times
        count = count + c
      }
      x = x + 1
    }
    count

  }
  def main(args: Array[String]): Unit = {
    println(countNature + countComplex)
  }
}