package projecteuler

object Problem155 {
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  def normalize(x: (Int, Int)): (Int, Int) = {
    val (n, d ) = x
    val g = gcd(n, d)
    (n / g, d / g)
  } 
  
  def parallel(x: (Int, Int), y: (Int, Int)): (Int, Int) = {
    val (n1, d1) = x
    val (n2, d2) = y
    normalize(n1 * d2 + n2 * d1, d1 * d2)
  }
  
  def serial(x: (Int, Int), y: (Int, Int)): (Int, Int) = {
    val (n1, d1) = x
    val (n2, d2) = y
    normalize(n1 * n2, n1 * d2 + n2 * d1)
  }
  
  val m = collection.mutable.Map(1 -> collection.mutable.Set((60, 1)))
  
  def isNew(n: Int, c: (Int, Int)): Boolean = 1 to n-1 forall (! m(_).contains(c))
  
  def count(n: Int): Unit = {
    val s = collection.mutable.Set.empty[(Int, Int)]
    (1 to n/2).foreach{a =>
      val b = n - a
      for (x <- m(a); y <- m(b)) {
        val se = serial(x, y)
        val pa = parallel(x, y)
        if (isNew(n, se)) s.add(se)
        if (isNew(n, pa)) s.add(pa)
      }
    }
    m(n) = s
  }
  
  def main(args: Array[String]): Unit = {
    val limit = 18
    val start = System.currentTimeMillis()
    2 to limit foreach count
    val result = 1 to limit map (m(_).size) sum
    
    println(result)
    println(s"Time used: ${System.currentTimeMillis() - start}ms")
  }
}