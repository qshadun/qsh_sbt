package projecteuler
/**
Lexicographic permutations
Problem 24
A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. 
If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
 */
object Problem24 {
  val letters = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).map(_.toString)
  def main(args: Array[String]): Unit = {
    val allPermutation = (for(
        c1 <- letters; val r1 = letters - c1;
        c2 <- r1; val r2 = r1 - c2;
        c3 <- r2; val r3 = r2 - c3;
        c4 <- r3; val r4 = r3 - c4;
        c5 <- r4; val r5 = r4 - c5;
        c6 <- r5; val r6 = r5 - c6;
        c7 <- r6; val r7 = r6 - c7;
        c8 <- r7; val r8 = r7 - c8;
        c9 <- r8; val r9 = r8 - c9;
        c10 <- r9
        ) yield(c1 + c2 + c3 + c4 + c5 + c6 + c7 + c8 + c9 + c10)).view
    println(allPermutation.take(1000000).last)
  }

}