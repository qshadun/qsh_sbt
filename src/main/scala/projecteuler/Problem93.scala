package projecteuler
//Arithmetic expressions
object Problem93 {
  def eval(a: Int, b: Int, op: String) =
    op match {
      case "+" => a + b
      case "-" => a - b
      case "*" => a * b
      case "/" if b != 0 && a % b == 0 => a / b
      case _ => throw new IllegalArgumentException
    }
  def possibleResult(ls: Seq[Int], ops: Seq[String]): Seq[Int] = ops match {
    case Nil => ls
    case _ =>
      val r = (1 to ls.size - 1).map { i =>
        possibleResult(ls.take(i), ops.take(i - 1)).map {n1 =>
          possibleResult(ls.drop(i), ops.drop(i)).map {n2 =>
            try {
              eval(n1, n2, ops(i-1))
            } catch {
              case _: Throwable => Int.MinValue
            }
          }
        }.flatten
      }.flatten
      r.filterNot(_ == Int.MinValue).distinct
  }
  val ops = List("+", "-", "*", "/")
  val opsCombo = for (
    o1 <- ops;
    o2 <- ops;
    o3 <- ops
  ) yield List(o1, o2, o3)
  def consecutiveNum(ns: Seq[Int]): (String, Int) = {
    val r = for (
      ns <- ns.permutations;
      os <- opsCombo
    ) yield possibleResult(ns, os)
    val pr = r.flatten.toList.filter(_ > 0).distinct.sorted
    val n = (0 until pr.length).find(i => pr(i) != i + 1)
    (ns.mkString, if (n.isDefined) n.get else pr.length)
  }
  def main(args: Array[String]): Unit = {
    val r = (0 to 9).combinations(4).map(consecutiveNum).toList
    val max = r.maxBy(_._2)._2
    val result = r.filter(_._2 == max)
    println(result)
  }
}