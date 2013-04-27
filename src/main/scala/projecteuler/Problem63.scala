package projecteuler
object Problem63 {
  def main(args: Array[String]) = {
    def pow(a: Int, b: Int): BigInt = if (b == 0) BigInt(1) else a * pow(a, b - 1)
    println((1 to 9).map(x => Stream.from(1).takeWhile(y => pow(x, y).toString.size == y)).map(_.size).sum)
  }
}