package projecteuler
//Prime pair connection
object Problem134 {
  import Utils._
  def toDigits(n: Int) = n.toString.map(_ - '0')
  def solve(x: Int, y: Int): Long = {
    val target = toDigits(x)
    val toMulti = toDigits(y)
    def recur(toMulti: Seq[Int], sofar: List[Int], part: Int, pos: Int): Int =
      if (toMulti.isEmpty || pos == target.size) sofar.mkString.toInt
      else {
        (0 to 9).map { x =>
          val newPart = x * toMulti.mkString.toInt * math.pow(10, pos).toInt + part
          val digits = toDigits(newPart)
          if (digits(digits.size - pos - 1) == target(target.size - pos - 1))
            recur(toMulti.tail, x :: sofar, newPart, pos + 1)
          else Int.MaxValue
        }.min
      }
    recur(toMulti, Nil, 0, 0).toLong * y
  }
  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis
    val sieve = Sieve(1100000)
    val primes = sieve.getPrimes.drop(2) // drop 2 and 3
    val toFind = primes.takeWhile(_ <= 1000000)
    val pairs = toFind.zip(toFind.tail :+ primes(toFind.size))
    println(pairs.map(pair => solve(pair._1, pair._2)).sum)
    println(s"Used time: ${System.currentTimeMillis - start}ms")
  }
}