import org.specs2.mutable._
import MaxCommonSequence._

class MaxCommonSequenceSpec extends SpecificationWithJUnit {

   "The MaxCommonSequence calculator" should {
      "get empty sequence when both sequence are empty" in {
        maxCommonSequence(Nil, Nil) must have size(0)
      }
      "get empty sequence when the first sequence is empty" in {
        maxCommonSequence(List(1,2,3), Nil) must have size(0)
      }
      "get empty sequence when the second sequence is empty" in {
        maxCommonSequence(Nil, List(1,2,3)) must have size(0)
      }
      
      "get the original sequence when both sequence are identical" in {
        val l = List(1,2,3,3,5,7,9)
        maxCommonSequence(l, l) must beEqualTo(l)
      }
      
      "the max common sequence of (1,2,3,4,5,6,7,8,9,10) and (2,3,5,9,10,25) is (2,3,5,9,10)" in {
        val l1 = List(1,2,3,4,5,6,7,8,9,10)
        val l2 = List(2,3,5,9,10,25)
        maxCommonSequence(l1, l2) must beEqualTo(List(2,3,5,9,10))
      }
      
      "sequence of identical elements" in {
        val l1 = List(1,1,1,1,1,1,1,1,1)
        val l2 = List(1,1,1,1)
        maxCommonSequence(l1, l2) must beEqualTo(l2)
      }
      
      "sequence of identical elements 2" in {
        val l1 = List(1,1,1,1,1,1,1,1,1)
        val l2 = List(1,1,2,1,2,1)
        maxCommonSequence(l1, l2) must beEqualTo(List(1,1,1,1))
      }
      
      "unordered sequence" in {
        val l1 = List(1,2,4,3,2,1,3,5,7,8,7,3)
        val l2 = List(4,2,9,1,2,1,5,8,9,2,1)
        maxCommonSequence(l1, l2) must beEqualTo(List(4, 2, 1, 5, 8))
      }
      
    }

}