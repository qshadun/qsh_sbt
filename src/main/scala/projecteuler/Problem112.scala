package projecteuler
//Bouncy numbers
object Problem112 {
  def isBouncy(n: Int) = {
    val s = n.toString
    val sorted = s.sorted
    s != sorted && s != sorted.reverse
  }
  def main(args: Array[String]): Unit = {
   var n = 100
   var bouncyNumber = 0
   var finded = false
   while(n > 0 && !finded) {
     n = n + 1
     if (isBouncy(n)) {
       bouncyNumber = bouncyNumber + 1
       finded = bouncyNumber * 100.toLong == n * 99.toLong
     }
   }
   println(n + ", " + finded)
  }
}