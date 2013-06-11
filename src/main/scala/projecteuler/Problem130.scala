package projecteuler
//Composites with prime repunit property
object Problem130 {
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  def bruteForce(n: Int) = {
    var x = 1
    while(BigInt(10).modPow(x, 9 * n) != 1) x = x + 1
    x
  }
  def main(args: Array[String]): Unit = {
    val target = if (args.isEmpty) 25 else args(0).toInt
    val compos = Stream.from(3).filter{x => 
      !BigInt(x).isProbablePrime(x) && gcd(x, 10) == 1 && (x - 1) %bruteForce(x) == 0
    }.take(target)
    println(compos.toList)
    println(compos.sum)
  }
}