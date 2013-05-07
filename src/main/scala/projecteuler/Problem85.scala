package projecteuler
//Counting rectangles
object Problem85 {
  def main(args: Array[String]): Unit = {
    def count(l: Int, w: Int) = (for(i <- 1 to l; j <- 1 to w) yield (l+1-i) * (w+1-j)).sum
    val bound = 2000000
    val result = Stream.from(1).map {w =>
      Stream.from(w).map{l =>
        (l, w, l * w, count(l, w))
      }.takeWhile(_._4 < bound)
    }.takeWhile(_.size > 0).flatten.maxBy(_._4)
    println(result)
  }
}