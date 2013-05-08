package projecteuler
//http://mathworld.wolfram.com/PythagoreanTriple.html
object Problem86 {
  val bound = 2000;
  val ar = Array.fill(bound + 1)(0);
  def transform(x: Int, y: Int, z: Int) {
    val (min, max) = if (x > y) (y, x) else (x, y)
    if (min <= bound && max <= 2 * bound) {
      if (max <= 2 * min) {
        (1 to bound / min).foreach { i =>
          val a = i * min
          val b = i * max
          var count = 0
          //careful here, b / 2 - (b - a) + 1 != a + 1 - b / 2 
          ar(a) = ar(a) + b / 2 - (b - a) + 1
        }
      }
      (1 to bound / max).foreach { i =>
        val a = i * max
        val b = i * min
        ar(a) = ar(a) + b / 2
      }
      transform(x - 2 * y + 2 * z, 2 * x - y + 2 * z, 2 * x - 2 * y + 3 * z);
      transform(x + 2 * y + 2 * z, 2 * x + y + 2 * z, 2 * x + 2 * y + 3 * z);
      transform(-x + 2 * y + 2 * z, -2 * x + y + 2 * z, -2 * x + 2 * y + 3 * z);
    }
  }
  def main(args: Array[String]): Unit = {
    transform(3, 4, 5)
    val solutions = ar.tail.scanLeft(0)(_ + _)
    println(solutions.zipWithIndex.find(_._1 >= 1000000))
  }
}