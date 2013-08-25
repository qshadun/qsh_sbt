package projecteuler

object Problem145 {

  def count(limit: Int) = {
    var count = 0
    var n = 1
    while (n <= limit) {
      if (check(n)) count = count + 1
      n = n + 1
    }
    count
  }
  def check(n: Int) = n % 10 != 0 && {
    val sum = n.toString.reverse.toInt + n
    sum.toString.forall{a =>
      (a - '0') % 2 != 0
    }
  }
  def main(args: Array[String]): Unit = {
    val limit = if (args.isEmpty) math.pow(10, 9).toInt else args(0).toInt
    println(count(limit))
  }

}