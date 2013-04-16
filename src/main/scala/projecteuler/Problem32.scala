package projecteuler
/**
Pandigital products
Problem 32
Published on Friday, 6th December 2002, 06:00 pm; Solved by 27885
We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; 
for example, the 5-digit number, 15234, is 1 through 5 pandigital.

The product 7254 is unusual, as the identity, 39 * 186 = 7254, containing multiplicand, multiplier, a
nd product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.

HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.
 */
object Problem32 {
  // ab * cde = fghi
  // a * bcde = fghi
  val digits = Range(1,10).map(_.toString).toList
  val pandigitals =  
    for(
        a <- digits;
        b <- digits - a;
        c <- digits - a - b;
        d <- digits - a - b - c;
        e <- digits - a - b - c - d;
        f <- digits - a - b - c - d - e;
        g <- digits - a - b - c - d - e - f;
        h <- digits - a - b - c - d - e - f - g;
        i <- digits - a - b - c - d - e - f - g - h;
        val n1 = (a + b).toInt;
        val n2 = (c + d + e).toInt;
        val n3 = (f + g + h + i).toInt;
        val n4 = a.toInt;
        val n5 = (b + c + d + e).toInt;
        val r:(Int, Int, Int) =  
          if (n1 * n2 == n3) (n1, n2, n3) 
          else 
            if(n4 * n5 == n3) (n4, n5, n3) else (0, 0, 0);
        if r != (0, 0, 0)
      ) yield(r)
  def main(args: Array[String]): Unit = {
    println(pandigitals)
    println(pandigitals.map(_._3).distinct.sum)
  }

}