package risk.shared.abstract0

import upickle.default.{macroRW, ReadWriter => RW}

abstract sealed class BattlePhase

object BattlePhase {
  implicit def rw: RW[BattlePhase] = macroRW
}

case object DeclareNoOfTroopsPhase extends BattlePhase
case object DiceThrowingPhase extends BattlePhase
case object ResultPhase extends BattlePhase

abstract class AbstractBattle(
                               val noOfAttackers: Int,
                               val noOfDefenders: Int = 0,
                               val phase: BattlePhase = DeclareNoOfTroopsPhase,
                               val attackerDieRolling: Boolean = false,
                               val defenderDieRolling: Boolean = false,
                               val attackerDie: Option[List[Int]] = None,
                               val defenderDie: Option[List[Int]] = None,
                 ) {
  def invariant(): Unit = {
    assert(noOfAttackers >= 0 && noOfAttackers <=3)
    if (attackerDie.isDefined) {
      assert(attackerDie.map(_.size).get == noOfAttackers)
    }
    if (defenderDie.isDefined) {
      assert(defenderDie.map(_.size).get == noOfDefenders)
    }
    phase match {
      case DeclareNoOfTroopsPhase => {
        assert(noOfDefenders == 0)
        assert(attackerDie.isEmpty && defenderDie.isEmpty)
      }
      case DiceThrowingPhase => {
        assert(noOfDefenders == 1 || noOfDefenders == 2)
        assert(attackerDie.isEmpty || defenderDie.isEmpty)
      }
      case ResultPhase => {
        assert(noOfDefenders == 1 || noOfDefenders == 2)
        assert(attackerDie.isDefined && defenderDie.isDefined)
      }
      case _ => {
        println("nothing to assert here...")
      }
    }
  }

  private lazy val pairs: List[(Int, Int)] = {
    assert(phase == ResultPhase)
    assert(attackerDie.isDefined)
    assert(defenderDie.isDefined)
    val a = attackerDie.get.sorted.reverse
    val d = defenderDie.get.sorted.reverse
    a.zip(d)
  }

  lazy val noOfTroopsLostByTheAttacker: Int = pairs.count(p => p._1 <= p._2)

  lazy val noOfTroopsLostByTheDefender: Int = pairs.count(p => p._1 > p._2)

  lazy val remainingAttackingTroops: Int = noOfAttackers - noOfTroopsLostByTheAttacker

  lazy val remainingDefendingTroops: Int = noOfDefenders - noOfTroopsLostByTheDefender

}
