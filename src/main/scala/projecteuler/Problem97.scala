package projecteuler
//Large non-Mersenne prime
object Problem97 {
  //28433*2^7830457+1
  def main(args: Array[String]): Unit = {
    def lastTen(n: Int) = {
      var exp = n
      var i = 1
      var result = 1L
      while(i <= n) {
        result = (result * 2L) % 10000000000L
        i = i + 1
      }
      result
    }
    val r = (28433 * lastTen(7830457) + 1).toString
    println(r.slice(r.length - 10, r.length))
  }
}