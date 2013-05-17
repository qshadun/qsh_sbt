package projecteuler
//Pandigital Fibonacci ends
object Problem104 {
  def fibo: Stream[BigInt] = {
    def loop(a: BigInt, b: BigInt): Stream[BigInt] = a #:: loop(b, a + b)
    loop(0, 1)
  }
  def main(args: Array[String]): Unit = {
    val digits = (1 to 9).map(_.toString.charAt(0)).toList
    def check(ss: String) = {
      val s = if (ss.length > 9) ss.substring(0, 9) else ss
      s.toCharArray.toList.sorted == digits
    }
    val n1 = fibo(2749)
    val n2 = fibo(2750)

    var a1 = n1.toString.substring(n1.toString.size - 9).toLong
    var a2 = n2.toString.substring(n2.toString.size - 9).toLong // for check last 9 digits

    var b1 = n1.toString.substring(0, 15).toLong
    var b2 = n2.toString.substring(0, 15).toLong
    var i = 2750
    while (!check(a2.toString) || !check(b2.toString)) {
      val t = a1 + a2
      a1 = a2
      a2 = if (t.toString.size > 9) t.toString.substring(t.toString.size - 9).toLong else t

      if (b1.toString.size == b2.toString.size) {
        val tt = b1 + b2
        b1 = b2
        b2 = tt
      } else {
        val tt = b1 + b2
        b1 = b2.toString.substring(0, 15).toLong
        b2 = tt.toString.substring(0, 15).toLong
      }
      i = i + 1
    }
    println(i)
  }
}