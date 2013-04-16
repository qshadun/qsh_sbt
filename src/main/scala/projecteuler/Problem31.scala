package projecteuler
/**
Problem 31
In England the currency is made up of pound, L, and pence, p, and there are eight coins in general circulation:

1p, 2p, 5p, 10p, 20p, 50p, L1 (100p) and L2 (200p).
It is possible to make L2 in the following way:

1L1 + 150p + 220p + 15p + 12p + 31p
How many different ways can L2 be made using any number of coins?
 */
object Problem31 {

  // 1, 2, 5, 10, 20, 50, 100, 200
  def findCombos(coins: List[Int], v: Int) = {
    def findR(used: List[Int], unUsed: List[Int], remain: Int): List[List[Int]] =
      if (unUsed.isEmpty && remain != 0) Nil
      else if (remain == 0) List((List.fill(unUsed.size)(0) ::: used).reverse)
      else {
        val r = for (i <- 0 to remain / unUsed.head) yield(findR(i :: used, unUsed.tail, remain - unUsed.head * i))
        r.flatten.toList
      }
    findR(Nil, coins, v)
  }
  def main(args: Array[String]): Unit = {
    val r = findCombos(List(1,2,5,10,20,50,100,200).reverse, 200)
    println(r.size)
    println(r.take(10))
  }

}