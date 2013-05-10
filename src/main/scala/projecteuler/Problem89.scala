package projecteuler

import scala.io.Source
// Roman number
object Problem89 {
  case class RomanDenomination(c: Char, n: Int) extends Ordered[RomanDenomination] {
    override def toString = c.toString
    override def compare(that: RomanDenomination) = this.n - that.n
  }
  /**
   * I = 1
   * V = 5
   * X = 10
   * L = 50
   * C = 100
   * D = 500
   * M = 1000
   * Only I, X, and C can be used as the leading numeral in part of a subtractive pair.
   * I can only be placed before V and X.
   * X can only be placed before L and C.
   * C can only be placed before D and M.
   */
  object RomanDenomination {
    val I = new RomanDenomination('I', 1)
    val V = new RomanDenomination('V', 5)
    val X = new RomanDenomination('X', 10)
    val L = new RomanDenomination('L', 50)
    val C = new RomanDenomination('C', 100)
    val D = new RomanDenomination('D', 500)
    val M = new RomanDenomination('M', 1000)

    def apply(c: Char) = c match {
      case I.c => I
      case V.c => V
      case X.c => X
      case L.c => L
      case C.c => C
      case D.c => D
      case M.c => M
    }
  }
  object RomanNumber {
    import RomanDenomination._
    def toInt(ds: Seq[RomanDenomination]): Int = {
      var n = 0
      var i = 0
      while (i < ds.size) {
        if (i < ds.length - 1 && ds(i) < ds(i + 1)) {
          n = n + ds(i + 1).n - ds(i).n
          i = i + 2
        } else {
          n = n + ds(i).n
          i = i + 1
        }
      }
      n
    }
    def toInt(s: String): Int = {
      toInt(s.map(RomanDenomination.apply))
    }
    def toMinimalForm(n: Int): String = {
      val sb = new StringBuilder()
      val (s1, r1) = partialParse(M, D, C, n)
      val (s2, r2) = partialParse(C, L, X, r1)
      val (s3, r3) = partialParse(X, V, I, r2)
      sb.append(s1).append(s2).append(s3)
      (1 to r3).foreach(i => sb.append(I))
      sb.toString
    }
    def partialParse(h: RomanDenomination, m: RomanDenomination, l: RomanDenomination, n: Int): (String, Int) = {
      val sb = new StringBuilder()
      var r = n
      (1 to r / h.n).foreach(i => sb.append(h.c))
      r = r % h.n
      if (r >= h.n - l.n) {
        sb.append(l.c).append(h.c)
        r = r - h.n + l.n
      } else {
        if (r >= m.n) {
          sb.append(m.c)
          r = r - m.n
        } else if (r >= m.n - l.n) {
          sb.append(l.c).append(m.c)
          r = r - m.n + l.n
        }
      }
      (sb.toString, r)
    }
  }
  def main(args: Array[String]): Unit = {
    val originalForms = Source.fromURL(getClass.getResource("/roman.txt")).getLines.toList
    import RomanNumber._
    val minalForms = originalForms.map(toInt).map(toMinimalForm)
    assert(originalForms.map(toInt) == minalForms.map(toInt))
    println(originalForms.map(_.size).sum - minalForms.map(_.size).sum)
  }
}