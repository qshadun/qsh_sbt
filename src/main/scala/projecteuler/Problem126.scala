package projecteuler
//Cuboid layers
object Problem126 {
  def layer(cub: (Int, Int, Int), n: Int) = {
    val (a, b, c) = cub
    val base = 2 * (a * b + b * c + c * a)
    val extra = (a + b + c) * (n - 1) * 4 + 4 * (n - 1) * (n - 2)
    base + extra
  }
  val limit = 100000
  val count = Array.fill(limit + 1)(0)
  for (a <- 1 to limit / 4; b <- 1 to math.min(a, limit / 6 / a); c <- 1 to math.min(b, (limit / 2 - a * b) / (a + b))) {
    var i = 1
    var number = 2 * (a * b + b * c + c * a)
    val l = a + b + c
    while (number < limit) {
      count(number) = count(number) + 1
      i = i + 1
      number = number + 4 * l + 8 * (i - 2)
    }
  }
  def main(args: Array[String]): Unit = {
    println(count.zipWithIndex.find(_._1 == 1000))
  }
}