package projecteuler
//Product-sum numbers
object Problem88 {
  val bound = 12000
  val ar = Array.fill(bound + 1)(0)
  def findSums(n: Int) = { // n is the number of numbers greater than 1
    def recur(ls: List[Int]): Unit = {
      if (ls.length == n - 1) {
        val p = ls.product
        val s = ls.sum
        var i = ls.head
        var continue = true
        do {
          val pr = p * i
          val su = s + i
          val k = pr - su + n
          if (k <= bound) {
            if (ar(k) == 0 || ar(k) > pr) ar(k) = pr
          } else
            continue = false
          i = i + 1
        } while (continue)
      } else {
        (ls.head to (bound / ls.product + 1)).foreach(x => recur(x :: ls))
      }
    }
    val max = math.pow(bound, 1d / n).toInt + 1
    (2 to max).foreach(x => recur(List(x)))
  }
  def log2(n: Int): Int = if (n == 1) 0 else 1 + log2(n >> 1)

  def main(args: Array[String]): Unit = {
    (log2(bound) to 2 by -1).map(findSums)
    println(ar.distinct.sum)
  }
}