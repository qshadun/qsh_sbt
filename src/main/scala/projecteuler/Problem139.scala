package projecteuler
//Pythagorean tiles
object Problem139 {
  def main(args: Array[String]): Unit = {
    val bound = if (args.isEmpty) 100000000 else args(0).toInt
    var count = 0
    def transform(x: Int, y: Int, z: Int): Unit = {
      val l = x + y + z
      if (z % (y - x) == 0 && l < bound) 
        count = count + bound/l
      if (l < bound) {
        transform(x - 2 * y + 2 * z, 2 * x - y + 2 * z, 2 * x - 2 * y + 3 * z);
        transform(x + 2 * y + 2 * z, 2 * x + y + 2 * z, 2 * x + 2 * y + 3 * z);
        transform(-x + 2 * y + 2 * z, -2 * x + y + 2 * z, -2 * x + 2 * y + 3 * z);
      }
    }
    transform(3, 4, 5)
    println(count)
  }
}