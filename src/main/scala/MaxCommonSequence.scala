object MaxCommonSequence {

  def maxCommonSequence[T](s1: Sequence[T], s2: Sequence[T]): Sequence[T] = {
    val init = (1 to (s1.length + 1)).foldLeft(List((0, List.empty[T]))) {(acc, x) => (0, List.empty[T]) :: acc }
    
    s2.foldLeft(init) {(acc, x) =>
      val current = acc.reverse
      s1.zip(current.zip(current.tail)).foldLeft(List((0, List.empty[T]))) { (step, e) =>
        val left = step.head
        val top = e._2._2
        val diag = e._2._1
        val c = 
          if (e._1 == x) (diag._1 + 1, x :: diag._2)
          else
            if (left._1 > top._1) left else top
        c :: step
      }
    }.head._2.reverse
  }
  def main(args: Array[String]): Unit = {
    val l1 = List(1,2,3,4,5,6)
    val l2 = List(1,3,5,8,9)
    println(maxCommonSequence(l1, l1))
    println(maxCommonSequence(l1, l2))
  }

}