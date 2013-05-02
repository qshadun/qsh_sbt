package projecteuler
object Problem76{
  def main(args: Array[String]) = {
    def summation(n: Int): Int = {
      def recur(i: Int, bound: Int, num: Int): Int = num match {
        case 1 => 1
        case _ =>
          (math.min(i-num+1, bound) to math.ceil(i.toDouble/num).toInt by -1).map{x =>
            recur(i - x, x, num -1)
          }.sum
      }
      (2 to n).map(recur(n, n, _)).sum
    }
    println(summation(100))
  }
}