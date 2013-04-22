package projecteuler
import scala.io.Source

object Problem54 {
  object Rank extends Enumeration {
    val HighCard, OnePair, TwoPairs, ThreeOfAKind, Straight, Flush, FullHouse, FourOfaKind, StraightFlush, RoyalFlush = Value
  }
  object Suit extends Enumeration{
    val Diamond, Club, Heart, Spade = Value
  }
  import Rank._
  import Suit._
  case class Card(value: Int, suit: Suit.Value) extends Ordered[Card]{
    override def toString = "(" + value + ", " + suit + ")"  
    def compare(that: Card) = 
        this.value - that.value
  }
  object Card {
    def apply(cs: String) = {
      val c1 = cs.charAt(0)
      val c2 = cs.charAt(1)
      val v = c1 match {
        case 'A'=> 14
        case 'K'=> 13
        case 'Q'=> 12
        case 'J'=> 11
        case 'T'=> 10
        case _ => c1.toInt - '0'
      }
      val suit = c2 match {
        case 'S' => Spade
        case 'H' => Heart
        case 'C' => Club
        case 'D' => Diamond
      }
      new Card(v, suit)
    }
  }
  
  case class Hand(cards: List[Card]) extends Ordered[Hand] {
    def isOnePair = cards.map(_.value).distinct.size == 4
    def isTwoPairs = {
      val g = cards.groupBy(x => x.value)
      g.size == 3 && g.exists(_._2.size == 2)
    }
    def isThreeOfAKind = {
      val g = cards.groupBy(x => x.value)
      g.size == 3 && g.exists(_._2.size == 3)
    }
    def isFullHouse = {
      val g = cards.groupBy(x => x.value)
      g.size == 2 && g.exists(_._2.size == 2)
    }
    def isFourOfAKind = {
      val g = cards.groupBy(x => x.value)
      g.size == 2 && g.exists(_._2.size == 4)
    }
    def isStraight = {
      val vs = cards.map(_.value)
      vs(1) == vs(0) - 1 &&
      vs(2) == vs(0) - 2 &&
      vs(3) == vs(0) - 3 &&
      vs(4) == vs(0) - 4
    }
    def isFlush = cards.map(_.suit).distinct.size == 1
    def isStraightFlush = isStraight && isFlush
    def isRoyalFlush =  isStraightFlush && cards(0).value == 10
    
    val rank: Rank.Value = 
      if (isRoyalFlush) RoyalFlush
      else if (isStraightFlush) StraightFlush
      else if (isFourOfAKind) FourOfaKind
      else if (isFullHouse) FullHouse
      else if (isFlush) Flush
      else if (isStraight) Straight
      else if (isThreeOfAKind) ThreeOfAKind
      else if (isTwoPairs) TwoPairs
      else if (isOnePair) OnePair
      else HighCard

    
    def toCompareList: List[Card] = rank match {
      case OnePair => 
        val pv = cards.groupBy(_.value).find(_._2.size == 2).get._1
        val (pair, notPair) = cards.partition(_.value == pv)
        pair ::: notPair
      case TwoPairs =>
        val sv = cards.groupBy(_.value).find(_._2.size == 1).get._1
        val (notPair, pair) = cards.partition(_.value == sv)
        pair ::: notPair
      case r if (r == ThreeOfAKind || r == FullHouse) =>
        val tv = cards.groupBy(_.value).find(_._2.size == 3).get._1
        val (three, notThree) = cards.partition(_.value == tv)
        three ::: notThree
      case FourOfaKind =>
        val fv = cards.groupBy(_.value).find(_._2.size == 4).get._1
        val (four, notFour) = cards.partition(_.value == fv)
        four ::: notFour
      case _ => cards
    }
    // assume two cards list are sorted descending
    def compareHighCard(c1: List[Card], c2: List[Card]) = {
      val diff = (0 until c1.size).find(i => c1(i).compareTo(c2(i)) != 0)
      if (diff.isDefined) c1(diff.get).compareTo(c2(diff.get))
      else 0
    }
    
    def compare(that: Hand) = 
      if (this.rank != that.rank) this.rank.id - that.rank.id
      else compareHighCard(this.toCompareList, that.toCompareList)
  }
  
  object Hand {
    def apply(ha: Seq[String]) = 
      new Hand(ha.map(Card(_)).toList.sorted.reverse)
  }
  
  case class Game(hand1: Hand, hand2: Hand) 
  object Game {
    def apply(gs: String) = {
      val (hs1, hs2) = gs.split(' ').splitAt(5)
      new Game(Hand(hs1), Hand(hs2))
    }
  }
  
  val input = Source.fromURL(getClass.getResource("/poker.txt")).getLines()
  
  def main(args: Array[String]): Unit = {
    println(input.map(Game(_)).foldLeft(0){(acc, g) =>
      if (g.hand1 > g.hand2) acc + 1 else acc
    })
  }
}