package projecteuler
//Arranged probability
object Problem100 {
  def findRep(n: Int) = {
    val root = math.sqrt(n).toInt
    def findR(sofar: List[Int], seen: List[(Int, Int)]): List[Int] = {
      val remain = seen.head._1
      val d = seen.head._2
      val nd = (n - remain * remain) / d
      val nr = root - (remain + root) % nd
      if (seen.size > 1 && (nr, nd) == seen.init.last) sofar.reverse else findR((remain + root) / nd :: sofar, (nr, nd) :: seen)
    }
    findR(Nil, List((root, 1)))
  }
  def conv(b0: Int, as: Seq[Int], bs: Seq[Int]): Stream[(BigInt, BigInt)] = {
    def loop(x0: (BigInt, BigInt), x1: (BigInt, BigInt), n: Int): Stream[(BigInt, BigInt)] = {
      val xn = (bs(n) * x1._1 + as(n) * x0._1, bs(n) * x1._2 + as(n) * x0._2)
      xn #:: loop(x1, xn, n + 1)
    }
    val x0 = (BigInt(b0), BigInt(1))
    val x1 = (bs.head * BigInt(b0) + as.head, BigInt(bs.head))
    x0 #:: x1 #:: loop(x0, x1, 1)
  }
  def convSqrt(n: Int): Stream[(BigInt, BigInt)] =
    conv(math.sqrt(n).toInt, Stream.continually(1), Stream.continually(findRep(n)).flatten)
  def main(args: Array[String]): Unit = {
    val bound = BigInt(10).pow(12)
    val cf = convSqrt(2)
    val i = Stream.from(1).find(i => cf(i)._1 * cf(i+1)._1 >= bound).get
    val k = cf(i)._1 * cf(i+1)._2
    val n = cf(i)._1 * cf(i+1)._1
    val n1 = k * (k + 1)
    val n2 = k * (k - 1)
    val d1 = n * (n + 1)
    val d2 = n * (n - 1)
    if (2 * n1 == d1 || 2 * n1 == d2) println(k + 1)
    else println(k)
  }
}