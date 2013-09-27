package projecteuler

object Problem151 {
  // A list of paper count of (a2, a3, a4, a5)
  val start = Seq(1, 1, 1, 1)

  val memo = scala.collection.mutable.Map.empty[Seq[Int], Rational]
  memo(Seq(0, 0, 0, 1)) = 0 //last a5 doesn't count
  def expectCount(pc: Seq[Int]): Rational =
    if (memo.contains(pc)) memo(pc)
    else {
      val total = pc.sum
      val exp =
        if (total == 1) {
          val (pre, after) = pc.span(_ == 0)
          val nPc = pre ++ (0 +: Seq.fill(after.size - 1)(1))
          1 + expectCount(nPc)
        } else {
          val noA5 = (0 to 2).map { i =>
            if (pc(i) == 0) Rational(0)
            else {
              val nPc = (pc.take(i) :+ (pc(i) - 1)) ++ pc.drop(i + 1).map(_ + 1)
              pc(i) * expectCount(nPc) / total
            }
          }.reduceLeft(_ + _)
          val a5 = if (pc(3) == 0) Rational(0)
          else {
            val nPc = pc.init :+ (pc(3) - 1)
            pc(3) * expectCount(nPc) / total
          }
          (noA5 + a5).normalize
        }
      memo(pc) = exp
      exp
    }

  def main(args: Array[String]): Unit = {
    val exp = expectCount(start)
    println(exp)
    println(exp.toDecimal)
  }
}