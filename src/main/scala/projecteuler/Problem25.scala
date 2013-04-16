package projecteuler
/**
1000-digit Fibonacci number
Problem 25
The Fibonacci sequence is defined by the recurrence relation:

Fn = Fn1 + Fn2, where F1 = 1 and F2 = 1.
Hence the first 12 terms will be:

F1 = 1
F2 = 1
F3 = 2
F4 = 3
F5 = 5
F6 = 8
F7 = 13
F8 = 21
F9 = 34
F10 = 55
F11 = 89
F12 = 144
The 12th term, F12, is the first term to contain three digits.

What is the first term in the Fibonacci sequence to contain 1000 digits?
 */
object Problem25 {
  val fibo: Stream[BigInt] = {
    def loop(a: BigInt, b: BigInt): Stream[BigInt] = a #:: loop(b, a+b)
    loop(BigInt(1), BigInt(1))
  }
  def main(args: Array[String]): Unit = {
    println(fibo.takeWhile(_.toString.length < 1000).size + 1)
  }

}