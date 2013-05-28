package projecteuler
//Square remainders
/**
This was a very quick one: (a+1)^n == an+1 (mod a^2), and (a-1)^n == an-1 or 1-an (mod a^2) depending whether n is odd or even; the sum is therefore either 2an or 2.

When a is odd, this is always maximised at a^2-a (as in the example with a=7), achieved for example when n=(a-1)/2; 
when a is even, it is maximised at a^2-2a for a>2, achieved for example when n=(a-2)/2.
 */
object Problem120 {
  def r(a: Int, n: Int) = if (n % 2 == 0) 2 else 2 * n * a % (a * a)  
  def rMax(a: Int) = //2 * a * ((a - 1)/2) % (a * a) 
    (1 to (2 * a - 1) by 2).map(r(a, _)).max
  def main(args: Array[String]): Unit = {
    println((3 to 1000).toList.map(rMax).sum)
  }
}