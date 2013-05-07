package projecteuler
/**
 * Community Chest (2/16 cards):
 * Advance to GO
 * Go to JAIL
 * Chance (10/16 cards):
 * Advance to GO
 * Go to JAIL
 * Go to C1
 * Go to E3
 * Go to H2
 * Go to R1
 * Go to next R (railway company)
 * Go to next R
 * Go to next U (utility company)
 * Go back 3 squares.
 */
object Problem84 {
  object Chest extends Enumeration {
    type Chest = Value
    val A2GChest, G2JChest, OtherChest = Value
  }
  object Chance extends Enumeration {
    type Chance = Value
    val A2G, G2J, G2C1, G2E3, G2H2, G2R1, G2NR, G2NU, GB3, Other = Value
  }
  import Chest._
  import Chance._
  val chestCards = A2GChest :: G2JChest :: List.fill(14)(OtherChest)
  val chanceCards = G2NR :: Chance.values.filterNot(_ == Other).toList ::: List.fill(6)(Other)

  var counter = Array.fill(40)(0)
  val POSITION_GO = 0
  val POSITION_JAIL = 10
  val POSITION_CHESTS = List(2, 17, 33)
  val POSITION_CHANCES = List(7, 22, 36)
  val POSITION_RAILS = List(5, 15, 25, 35)
  val POSITION_UTILITIS = List(12, 28)
  def simulate(n: Int) = { // simulate n turns
    val random = scala.util.Random
    var chests = random.shuffle(chestCards)
    var chances = random.shuffle(chanceCards)
    var pos = 0
    var doubleCount = 0
    for (i <- 1 to n) {
      val roll1 = random.nextInt(4) + 1
      val roll2 = random.nextInt(4) + 1
      if (roll1 == roll2)
        doubleCount = doubleCount + 1
      else
        doubleCount == 0
      if (doubleCount == 3) {
        pos = POSITION_JAIL
        doubleCount == 0
      } else {
        pos = (pos + roll1 + roll2) % 40
        if (POSITION_CHESTS.contains(pos)) {
          pos = handleChest(chests.head, pos)
          chests = chests.tail ::: List(chests.head)
        } else if (POSITION_CHANCES.contains(pos)) {
          pos = handleChance(chances.head, pos)
          chances = chances.tail ::: List(chances.head)
        }else if (pos == 30){ // go to jail
          pos = POSITION_JAIL
        }
      }
      counter(pos) = counter(pos) + 1
    }
  }
  def next(n: Int, postions: Seq[Int]) = {
    val r = postions.find(_ > n)
    if (r.isDefined) r.get else postions.head
  }
  def handleChance(c: Chance, n: Int): Int = c match {
    case A2G => POSITION_GO
    case G2J => POSITION_JAIL
    case G2C1 => 11
    case G2E3 => 24
    case G2H2 => 39
    case G2R1 => 5
    case G2NR => next(n, POSITION_RAILS)
    case G2NU => next(n, POSITION_UTILITIS)
    case GB3 => if (n >= 3) n - 3 else n + 40 - 3
    case _ => n
  }
  def handleChest(c: Chest, n: Int): Int = c match {
    case A2GChest => POSITION_GO
    case G2JChest => POSITION_JAIL
    case _ => n
  }
  def main(args: Array[String]): Unit = {
    val games = 1000
    val turns = 1000
    (1 to games).foreach(x => simulate(turns))
    println(counter.zipWithIndex.sortWith(_._1 > _._1).map{case (p, i) => (p.toDouble/ (games * turns), i)}.toList)
  }
}