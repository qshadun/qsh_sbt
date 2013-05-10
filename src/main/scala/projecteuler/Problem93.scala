package projecteuler

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
      val r1 = try {
        possibleResult(eval(ls(0), ls(1), ops.head) +: ls.tail.tail, ops.tail)
      } catch {
        case _: Throwable => Nil
      }
      val r2 = possibleResult(ls.tail, ops.tail).map {x =>
        try {
          eval(ls.head, x, ops.head)
        } catch {
          case _: Throwable => Int.MinValue
        }
      }
      (r1 ++ r2.filterNot(_ == Int.MinValue)).distinct
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
    val r = (0 to 9).combinations(4).map(consecutiveNum).maxBy(_._2)
    println(r)
  }
}