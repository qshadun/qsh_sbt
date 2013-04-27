package projecteuler
object Problem64 {
  def main(args: Array[String])  = {
    def isPerfectSquare(n: Int) = {
      val root = math.sqrt(n).toInt
      root * root == n
    }
    val nums = (2 to 10000).filter(!isPerfectSquare(_))
    def findRep(n: Int) = {
      val root = math.sqrt(n).toInt
      def findR(sofar: List[Int], seen: List[(Int,Int)]): List[Int] = {
        val remain = seen.head._1
        val d = seen.head._2
        val nd = (n - remain * remain) / d
        val nr = root - (remain + root) % nd
        if (seen.size > 1 && (nr, nd) == seen.init.last) sofar.reverse else findR((remain + root)/nd :: sofar, (nr, nd) :: seen)
      }
      findR(Nil, List((root, 1)))
    }
    println(nums.map(findRep).filter(_.size % 2 != 0).size)
  }
}