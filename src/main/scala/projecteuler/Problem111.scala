package projecteuler
//Primes with runs
object Problem111 {
  def isPrime(n: BigInt) = {
    val s = n.toString
    !(s.endsWith("0") || s.endsWith("2") || s.endsWith("5") || s.map(_ - '0').sum % 3 == 0) &&
      n.isProbablePrime(20)
  }
  def calc(numOfDigits: Int, digit: Int): (Int, Int, BigInt) = {//(M, N, S)
    def generateNumbers(rep: Int): List[BigInt] = {
      def recur(sofar: List[Int], count: Int): List[BigInt] =
        if (count < 0 || sofar.size == numOfDigits && sofar.head == 0) Nil
        else {
          if (sofar.size == numOfDigits && count == 0)
            List(BigInt(sofar.mkString))
          else {
            if (count > numOfDigits - sofar.size) Nil
            else {
              List.tabulate(10)(identity).map { i =>
                if (i == digit)
                  recur(i +: sofar, count - 1)
                else recur(i +: sofar, count)
              }.flatten
            }
          }

        }
      recur(Nil, rep)
    }
    var finded = false
    var i = numOfDigits - 1
    var result = (0, 0, BigInt(0))
    while (!finded && i > 1) {
      val primes = generateNumbers(i).filter(isPrime)
      if (primes.size > 0) {
        result = (i, primes.size, primes.sum)
        finded = true
      }
      i = i - 1
    }
    result
  }
  def solve(numOfDigits: Int) = List.tabulate(10)(identity).map(calc(numOfDigits, _))
  def main(args: Array[String]): Unit = {
    val result = solve(10)
    println(result)
    println(result.map(_._3).sum)
  }
}