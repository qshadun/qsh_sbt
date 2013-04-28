package projecteuler
/**
Square root convergents
Problem 57
It is possible to show that the square root of two can be expressed as an infinite continued fraction.

 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...

By expanding this for the first four iterations, we get:

1 + 1/2 = 3/2 = 1.5
1 + 1/(2 + 1/2) = 7/5 = 1.4
1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...

The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985, 
is the first example where the number of digits in the numerator exceeds the number of digits in the denominator.

In the first one-thousand expansions, how many fractions contain a numerator with more digits than denominator?
 */
object Problem57 {
  def main(args: Array[String]): Unit = {
    def iterate(r: Rational) = (1 + 1 / (1 + r)).normalize
    println((1 to 1000).foldLeft((Rational(1), 0)){(acc, x)=>
      val it = iterate(acc._1)
      if (it.n.toString.size > it.d.toString.size) (it, acc._2 + 1) else (it, acc._2)
    })
  }
}