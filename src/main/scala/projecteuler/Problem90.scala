package projecteuler
//Cube digit pairs
object Problem90 {
  //01, 04, 09, 16, 25, 36, 49, 64, and 81.
  val squares = List("01", "04", "09", "16", "25", "36", "49", "64", "81").map(_.toCharArray)
  def check(c1: Seq[Char], c2: Seq[Char]): Boolean =
    squares.forall{s => 
      c1.contains(s(0)) && c2.contains(s(1)) ||
      c1.contains(s(1)) && c2.contains(s(0))
  }
  def extend(ls: Seq[Char]) = 
    if (ls.contains('6') && ! ls.contains('9')) '9' +: ls
    else if (ls.contains('9') && ! ls.contains('6')) '6' +: ls
    else ls
  
  def main(args: Array[String]): Unit = {
     val digits = (0 to 9).map(_ + '0').map(_.toChar).toList
     val combinations = digits.combinations(6).toVector
     val r = for (
           i1 <- 0 until combinations.size;
           i2 <- i1 until combinations.size;
           c1 = combinations(i1);
           c2 = combinations(i2);
           if check(extend(c1), extend(c2))
         ) yield(c1, c2)
     println(r.size)
  }
}