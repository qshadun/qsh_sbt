package projecteuler
/**
 * List all the reverse order pairs and put them in each possible position
 */
object Problem158 {
  val ns = 1 to 26 toSeq

  val revOrders = ns.flatMap{i =>
    ns.filter(_ < i).map(Seq(i, _))
  }
  
  def count(n: Int) = {
    (0 to (n-2)).map {i =>
      revOrders.map {
        case Seq(h, t) =>
          val before = t - 1
          val after = 26 - h
          val share = h - t - 1
          (0 to math.min(share, i)).map{j =>
            com(share, j) * com(before, i - j) * com(after + share - j, n - 2 - i)
          }.sum
      }.sum
    }.sum
  }
  
  def com(n: Int, i: Int): Long = {
    if (i == 0 || n == i) 1
    else if (n < i) 0
    else (((n-i+1) to n).map(BigInt.apply).product / (1 to i).map(BigInt.apply).product).longValue 
  }
  
  def main(args: Array[String]): Unit = {
    val counts = 1 to 26 map count
    println(counts)
    println(counts.max)
    println(counts.indexOf(counts.max) + 1)
  }
}